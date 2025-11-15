use axum::{
    Form, Router,
    extract::{Path, State},
    http::StatusCode,
    response::{IntoResponse, Result},
    routing::{get, patch, put},
};
use hypertext::prelude::*;
use sqlx::SqlitePool;
use tokio::net::TcpListener;

#[tokio::main]
async fn main() {
    let db = init_db().await.unwrap();

    let listener = TcpListener::bind("127.0.0.1:9999").await.unwrap();
    let router = Router::new()
        .route("/", get(index))
        .route("/add", put(add_todo))
        .route("/update/{id}", patch(update_todo))
        .with_state(db);

    axum::serve(listener, router).await.unwrap();
}

async fn init_db() -> Result<SqlitePool, sqlx::Error> {
    let pool = SqlitePool::connect("sqlite:todos.db").await?;

    sqlx::migrate!();

    Ok(pool)
}

#[derive(sqlx::FromRow, Clone)]
struct Todo {
    id: i64,
    text: String,
    done: bool,
}

#[derive(serde::Deserialize)]
struct NewTodo {
    text: String,
}

#[component]
fn todo_item<'a>(todo: &'a Todo) -> impl Renderable {
    maud! {
        li {
            input #{"todo-" (todo.id)} type="checkbox" checked[todo.done]
            name="id" value=(todo.id) hx-patch={"/update/" (todo.id)} hx-target="closest li" hx-swap="outerHTML";
            label for={"todo-" (todo.id)} {(todo.text)}
        }
    }
}

async fn add_todo(db: State<SqlitePool>, todo: Form<NewTodo>) -> Result<impl IntoResponse> {
    let text = todo.text.clone();

    let todo_id = sqlx::query!(
        "INSERT INTO todos (text, done) VALUES (?, ?) RETURNING id",
        text,
        false
    )
    .fetch_one(&*db)
    .await
    .map_err(|e| {
        eprintln!("Database error: {}", e);
        StatusCode::INTERNAL_SERVER_ERROR.into_response()
    })?;

    Ok(maud! {
        TodoItem todo=(&Todo {
            id: todo_id.id,
            text: text.to_owned(),
            done: false,
        });
    })
}

async fn update_todo(
    db: State<SqlitePool>,
    Path(todo_id): Path<i64>,
    body: String,
) -> Result<impl IntoResponse> {
    // The body only includes the checkbox value if it is checked.
    let done = !body.is_empty();

    let todo = sqlx::query_as!(
        Todo,
        "UPDATE todos SET done = ? WHERE id = ? RETURNING *",
        done,
        todo_id
    )
    .fetch_one(&*db)
    .await
    .map_err(|e| {
        eprintln!("Database error: {}", e);
        StatusCode::INTERNAL_SERVER_ERROR.into_response()
    })?;

    Ok(maud! {
        TodoItem todo=(&todo);
    })
}

async fn index(db: State<SqlitePool>) -> impl IntoResponse {
    let todos = sqlx::query_as!(Todo, "SELECT * FROM todos")
        .fetch_all(&*db)
        .await
        .unwrap();

    maud! {
        script src="https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js" integrity="sha384-/TgkGk7p307TH7EXJDuUlgG3Ce1UVolAOFopFekQkkXihi5u/6OCvVKyz1W+idaz" crossorigin="anonymous" {}
        div {
            h1 { "Todo List" }
            form hx-put="/add" hx-target="#todo-list" hx-swap="beforeend" {
                input name="text" placeholder="New todo" required;
                button type="submit" { "Add Todo" }
            }
            ul id="todo-list" {
                @for todo in &todos {
                    TodoItem todo=(todo);
                }
            }
        }
    }
}
