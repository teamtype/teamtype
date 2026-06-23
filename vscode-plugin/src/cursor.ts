// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
// SPDX-FileCopyrightText: 2026 Mohamed El tamawey <tammwy22@gmail.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import * as vscode from "vscode"

const selectionDecorationType = vscode.window.createTextEditorDecorationType({
    backgroundColor: "#1a4978",
    borderRadius: "0.1rem",
    rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed,
    before: {
        color: "#548abf",
        contentText: "ᛙ",
        margin: "0px 0px 0px -0.35ch",
        textDecoration: "font-weight: bold; position: absolute; top: 0; font-size: 200%; z-index: 0;",
    },
})

interface RemoteCursor {
    name: string
    uri: vscode.Uri
    selection: vscode.DecorationOptions
}

let cursors: Map<string, RemoteCursor[]> = new Map()
let followedUser: string | undefined = undefined

export function setCursor(userid: string, name: string, uri: vscode.Uri, selections: vscode.DecorationOptions[]) {
    let usersCursors = cursors.get(userid)
    if (usersCursors) {
        // Remove all decorations by this user.
        for (let cursor of usersCursors) {
            // TODO: Refactor this into drawCursors below?
            const editors = vscode.window.visibleTextEditors.filter(
                (editor) => editor.document.uri.toString() === cursor.uri.toString(),
            )
            for (const editor of editors) {
                editor.setDecorations(selectionDecorationType, [])
            }
        }
    }

    let newCursors = selections.map((s) => {
        return {name, selection: s, uri}
    })
    cursors.set(userid, newCursors)

    const editors = vscode.window.visibleTextEditors.filter(
        (editor) => editor.document.uri.toString() === uri.toString(),
    )
    for (let editor of editors) {
        drawCursors(editor)
    }
}

export function getCursorInfo(): string {
    if (cursors.size == 0) {
        return "(No cursors.)"
    } else {
        let message: string[] = []
        cursors.forEach((usersCursors, _userid) => {
            for (let cursor of usersCursors) {
                let line1 = cursor.selection.range.start.line + 1
                let line2 = cursor.selection.range.end.line + 1
                if (line1 > line2) {
                    ;[line1, line2] = [line2, line1]
                }
                let position = line1 == line2 ? `${line1}` : `${line1}-${line2}`

                // TODO: Trim URI to the relevant parts inside the project.
                message.push(`${cursor.name} @ ${cursor.uri}:${position}`)
            }
        })
        return message.join("\n")
    }
}

export function drawCursors(editor: vscode.TextEditor | undefined) {
    if (editor) {
        let uri = editor.document.uri
        let allSelections = Array.from(cursors.values())
            .flat()
            .filter((cursor) => cursor.uri.toString() === uri.toString())
            .map((cursor) => cursor.selection)
        editor.setDecorations(selectionDecorationType, allSelections)
    }
}

/// this function does nothing if no cursor to follow
export async function syncFollowCursor() {
    try {
        if (followedUser === undefined) {
            return
        }
        const cursorsToFollow = cursors.get(followedUser)
        if (!cursorsToFollow || cursorsToFollow.length === 0) {
            return
        }
        const cursorToFollow = cursorsToFollow.at(-1) as (typeof cursorsToFollow)[number] // follow last cursor like the neovim plugin
        const textDocument = await vscode.workspace.openTextDocument(cursorToFollow.uri)
        const editor = await vscode.window.showTextDocument(textDocument, {preview: false})
        editor.selection = new vscode.Selection(
            cursorToFollow.selection.range.start,
            cursorToFollow.selection.range.end,
        )
        editor.revealRange(cursorToFollow.selection.range, vscode.TextEditorRevealType.InCenterIfOutsideViewport)
    } catch {}
}
export async function followCursor() {
    const items: vscode.QuickPickItem[] = [
        {label: "$(circle-slash) Stop following", description: "__stop__", alwaysShow: true},
    ]

    cursors.forEach((remoteCursors, uid) => {
        if (remoteCursors.length > 0) {
            const label = followedUser === uid ? `$(link) ${remoteCursors[0].name}` : remoteCursors[0].name
            items.push({label, description: uid})
        }
    })

    const picked = await vscode.window.showQuickPick(items, {
        placeHolder: "Follow which user?",
    })

    if (!picked) {
        return
    }

    const useridToFollow = picked.description !== "__stop__" ? picked.description : undefined
    followedUser = useridToFollow
    await syncFollowCursor()
}
