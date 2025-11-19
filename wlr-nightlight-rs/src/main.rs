use std::{
    io::{Seek, SeekFrom, Write},
    os::fd::AsFd,
    thread,
    time::Duration,
};
use wayland_client::{
    Connection, Dispatch, QueueHandle, delegate_noop,
    protocol::{
        wl_output::{self},
        wl_registry::{self},
    },
};
use wayland_protocols_wlr::gamma_control::v1::client::{
    zwlr_gamma_control_manager_v1 as wlr_gamma_control_manager,
    zwlr_gamma_control_v1 as wlr_gamma_control,
};

// The main function of our program
fn main() {
    // Create a Wayland connection by connecting to the server through the
    // environment-provided configuration.
    let conn = Connection::connect_to_env().unwrap();

    // Retrieve the WlDisplay Wayland object from the connection. This object is
    // the starting point of any Wayland program, from which all other objects will
    // be created.
    let display = conn.display();

    // Create an event queue for our event processing
    let mut event_queue = conn.new_event_queue();
    // And get its handle to associate new objects to it
    let qh = event_queue.handle();

    // Create a wl_registry object by sending the wl_display.get_registry request.
    // This method takes two arguments: a handle to the queue that the newly created
    // wl_registry will be assigned to, and the user-data that should be associated
    // with this registry (here it is () as we don't need user-data).
    let _registry = display.get_registry(&qh, ());

    // Create our initial state object.
    let mut state = AppData::default();

    // At this point everything is ready, and we just need to wait to receive the events
    // from the wl_registry. Our callback will print the advertised globals.

    // To actually receive the events, we invoke the `roundtrip` method. This method
    // is special and you will generally only invoke it during the setup of your program:
    // it will block until the server has received and processed all the messages you've
    // sent up to now.
    //
    // In our case, that means it'll block until the server has received our
    // wl_display.get_registry request, and as a reaction has sent us a batch of
    // wl_registry.global events.
    //
    // `roundtrip` will then empty the internal buffer of the queue it has been invoked
    // on, and thus invoke our `Dispatch` implementation that prints the list of advertised
    // globals.
    event_queue.roundtrip(&mut state).unwrap();
    event_queue.roundtrip(&mut state).unwrap();
    event_queue.roundtrip(&mut state).unwrap();

    dbg!(state);

    thread::sleep(Duration::from_secs(1));
}

// This struct represents the state of our app. This simple app does not
// need any state, but this type still supports the `Dispatch` implementations.
#[derive(Debug, Default)]
struct AppData {
    gamma_control_manager: Option<wlr_gamma_control_manager::ZwlrGammaControlManagerV1>,
    outputs: Vec<Output>,
}

#[derive(Debug)]
struct Output {
    wl_output: wl_output::WlOutput,
    gamma_control: Option<wlr_gamma_control::ZwlrGammaControlV1>,
    gamma_size: Option<u32>,
}

impl AppData {
    fn update_outputs(&mut self, qh: &QueueHandle<Self>) {
        let Some(ref gamma_control_manager) = self.gamma_control_manager else {
            return;
        };

        for (idx, output) in self.outputs.iter_mut().enumerate() {
            if output.gamma_control.is_some() {
                continue;
            }

            let gamma_control = gamma_control_manager.get_gamma_control(&output.wl_output, qh, idx);

            dbg!(&gamma_control);

            output.gamma_control = Some(gamma_control);
        }
    }

    fn update_gamma(&mut self, output_idx: usize) {
        let Output {
            gamma_control,
            gamma_size,
            ..
        } = &self.outputs[output_idx];
        let gamma_control = gamma_control
            .as_ref()
            .expect("Gamma control object should be present");
        let gamma_size = gamma_size.expect("Gamma size should be present");

        let gamma_table = vec![20; gamma_size as usize * 3 * 2];

        let opts = memfd::MemfdOptions::default();
        let mfd = opts.create("wlr-nightlight-rs-gamma-table").unwrap();

        let mut file = mfd.into_file();
        file.write_all(&gamma_table).unwrap();
        file.seek(SeekFrom::Start(0)).unwrap();

        gamma_control.set_gamma(file.as_fd());
    }
}

// Implement `Dispatch<WlRegistry, ()> for our state. This provides the logic
// to be able to process events for the wl_registry interface.
//
// The second type parameter is the user-data of our implementation. It is a
// mechanism that allows you to associate a value to each particular Wayland
// object, and allow different dispatching logic depending on the type of the
// associated value.
//
// In this example, we just use () as we don't have any value to associate. See
// the `Dispatch` documentation for more details about this.
impl Dispatch<wl_registry::WlRegistry, ()> for AppData {
    fn event(
        state: &mut Self,
        registry: &wl_registry::WlRegistry,
        event: wl_registry::Event,
        _: &(),
        _: &Connection,
        qh: &QueueHandle<AppData>,
    ) {
        // When receiving events from the wl_registry, we are only interested in the
        // `global` event, which signals a new available global.
        // When receiving this event, we just print its characteristics in this example.
        if let wl_registry::Event::Global {
            name,
            interface,
            version,
        } = event
        {
            // println!("[{}] {} (v{})", name, interface, version);

            match interface.as_str() {
                "wl_output" => {
                    println!("Fetching wl_output {name}");

                    let wl_output: wl_output::WlOutput = registry.bind(name, version, qh, ());
                    dbg!(&wl_output);

                    state.outputs.push(Output {
                        wl_output,
                        gamma_control: None,
                        gamma_size: None,
                    });

                    state.update_outputs(qh);
                }
                "zwlr_gamma_control_manager_v1" => {
                    println!("Fetching zwlr_gamma_control_manager_v1 {name}");

                    let gamma_control = registry.bind(name, version, qh, ());

                    dbg!(&gamma_control);

                    state.gamma_control_manager = Some(gamma_control);

                    state.update_outputs(qh);
                }
                _ => {}
            }
        }
    }
}

impl Dispatch<wlr_gamma_control::ZwlrGammaControlV1, usize> for AppData {
    fn event(
        state: &mut Self,
        _proxy: &wlr_gamma_control::ZwlrGammaControlV1,
        event: wlr_gamma_control::Event,
        output_idx: &usize,
        _conn: &Connection,
        _qh: &QueueHandle<Self>,
    ) {
        match event {
            wlr_gamma_control::Event::GammaSize { size } => {
                state.outputs[*output_idx].gamma_size = Some(size);
                state.update_gamma(*output_idx);
            }
            wlr_gamma_control::Event::Failed => {
                todo!()
            }
            _ => {
                unimplemented!()
            }
        }
    }
}

delegate_noop!(AppData: ignore wl_output::WlOutput);
// TODO
delegate_noop!(AppData: ignore wlr_gamma_control_manager::ZwlrGammaControlManagerV1);
