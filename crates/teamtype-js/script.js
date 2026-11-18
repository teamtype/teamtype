import init, {launch} from "./pkg/teamtype.js"

let log = (message) => {
    const p = document.createElement("p")
    p.innerText = "LOG: " + message
    document.body.append(p)
}
let inform = (message) => {
    const p = document.createElement("p")
    p.innerText = "INFO: " + message
    document.body.append(p)
}
let warn = (message) => {
    const p = document.createElement("p")
    p.innerText = "WARN: " + message
    document.body.append(p)
}
let msg = (message) => {
    const p = document.createElement("p")
    p.innerText = "MSG: " + message
    document.body.append(p)
}

init().then(async () => {
    const editor_connection = await launch(msg, log, inform, warn)

    const ta = document.querySelector("textarea")
    ta.value = ""
    let previous = ""

    await editor_connection.send("open", {relative_uri: "file", content: ""})

    ta.oninput = (e) => {
        const changes = Diff.diffChars(previous, ta.value)

        previous = ta.value
    }

    ta.onselectionchange = async () => {
        await editor_connection.send("cursor", {
            relative_uri: "file",
            ranges: [
                {
                    start: {line: 0, character: ta.selectionStart},
                    end: {line: 0, character: ta.selectionEnd},
                },
            ],
        })
    }
})
