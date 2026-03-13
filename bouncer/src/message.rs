pub enum Message {
    ToggleCleaning,
    OpenBrowserPicker,
    CloseBrowserPicker,
    SelectNext,
    SelectPrevious,
    ConfirmSelection,
    Quit,
}

pub enum Action {
    None,
    Quit,
    OpenUrl { exec: String, url: String },
}
