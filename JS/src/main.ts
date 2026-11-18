import { fromEvent, merge } from "rxjs";
import { map, mergeScan, first } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const updateTitleButton = document.getElementById("update-title")!;

const updateMessage = document.getElementById("save-message")!;

const saveHTMLButton = document.getElementById("save-html")!;

const checkbox = document.querySelector('input[name="checkbox"]')!;

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;

const titleInput = document.getElementById(
    "title-input",
) as HTMLTextAreaElement;

type Action = (_: State) => State;

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, markdown: value, save: false })),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value, save: false })),
);

// Create an Observable for clicking the update title button
const updateTitle$: Observable<Action> = fromEvent(
    updateTitleButton,
    "click",
).pipe(map((_) => (s) => ({ ...s, title: titleInput.value, save: false })));

// Create an Observable for clicking the saving HTML button
const saveHTML$: Observable<Action> = fromEvent(saveHTMLButton, "click").pipe(
    map((_) => (s) => ({ ...s, save: true })),
);

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
            save: String(s.save), //Header for saving
            title: s.title, //Header for title
        },
        body: s.markdown,
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            return {
                ...s,
                HTML: data.html,
            };
        }),
        first(),
    );
}

const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,
    title: "Converted HTML",
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(input$, checkboxStream$, updateTitle$, saveHTML$)
        .pipe(
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return getHTML(newState);
            }, initialState),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById("html-output");
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                if (value.renderHTML) {
                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                    htmlOutput.innerHTML = highlight + value.HTML;
                    // Magic code to add code highlighting
                    const blocks = htmlOutput.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                } else {
                    htmlOutput.textContent = value.HTML;
                }
            }
            value.save
                ? (updateMessage.textContent = "HTML HAS BEEN SAVED")
                : (updateMessage.textContent = "");
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
