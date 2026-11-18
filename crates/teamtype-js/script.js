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

// Convert jsdiff changes to Teamtype delta.
function changesToDelta(changes) {
    let delta = []
    let n = 0
    for (const c of changes) {
        if (c.added) {
            const range = {
                start: {line: 0, character: n},
                end: {line: 0, character: n},
            }
            delta.push({range: range, replacement: c.value})
        } else if (c.removed) {
            const range = {
                start: {line: 0, character: n},
                end: {line: 0, character: n + c.count},
            }
            delta.push({range: range, replacement: ""})
            n += c.count
        } else {
            // Assuming: Unchanged part.
            n += c.count
        }
    }
    return delta
}

init().then(async () => {
    const ta = document.querySelector("textarea")
    ta.value = ""
    let previous = ""
    let daemon_revision = 0
    let editor_revision = 0

    let msg = (message) => {
        console.log(message)
        const p = document.createElement("p")
        p.innerText = "MSG: " + JSON.stringify(message)
        document.body.append(p)

        if (
            message["method"] == "edit" &&
            message["params"]["relative_uri"] == "file" &&
            message["params"]["revision"] == editor_revision
        ) {
            daemon_revision += 1
            for (const d of message["params"]["delta"]) {
                console.log(d)
                const sc = d["range"]["start"]["character"]
                const ec = d["range"]["end"]["character"]
                // TODO maybe apply the deltas from back to front?
                previous =
                    previous.slice(0, sc) +
                    d["replacement"] +
                    previous.slice(ec)
            }
            ta.value = previous
        }
    }

    const editor_connection = await launch(msg, log, inform, warn)

    await editor_connection.send("open", {relative_uri: "file", content: ""})

    ta.oninput = (e) => {
        const changes = Diff.diffChars(previous, ta.value)
        const delta = changesToDelta(changes)
        console.log(delta)
        editor_connection.send("edit", {
            relative_uri: "file",
            revision: daemon_revision,
            delta: delta,
        })
        editor_revision += 1

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
