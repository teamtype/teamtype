// SPDX-FileCopyrightText: 2024 blinry <mail@blinry.org>
// SPDX-FileCopyrightText: 2024 zormit <nt4u@kpvn.de>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

import * as vscode from "vscode";

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
});

let cursors: Map<string, RemoteCursor[]> = new Map();
let useridToFollow: string | null = null;

export function setCursor(userid: string, name: string, uri: vscode.Uri, selections: vscode.DecorationOptions[]) {
  let usersCursors = cursors.get(userid);
  if (usersCursors) {
    // Remove all decorations by this user.
    for (let cursor of usersCursors) {
      // TODO: Refactor this into drawCursors below?
      const editors = vscode.window.visibleTextEditors.filter(
        (editor) => editor.document.uri.toString() === cursor.uri.toString(),
      );
      for (const editor of editors) {
        editor.setDecorations(selectionDecorationType, []);
      }
    }
  }

  let newCursors = selections.map((s) => {
    return { name, selection: s, uri };
  });
  cursors.set(userid, newCursors);

  const editors = vscode.window.visibleTextEditors.filter(
    (editor) => editor.document.uri.toString() === uri.toString(),
  );
  for (let editor of editors) {
    drawCursors(editor);
  }
}

export function getCursorInfo(): string {
  if (cursors.size == 0) {
    return "(No cursors.)";
  } else {
    let message: string[] = [];
    cursors.forEach((usersCursors, _userid) => {
      for (let cursor of usersCursors) {
        let line1 = cursor.selection.range.start.line + 1;
        let line2 = cursor.selection.range.end.line + 1;
        if (line1 > line2) {
          ;[line1, line2] = [line2, line1];
        }
        let position = line1 == line2 ? `${line1}` : `${line1}-${line2}`;

        // TODO: Trim URI to the relevant parts inside the project.
        message.push(`${cursor.name} @ ${cursor.uri}:${position}`);
      }
    });
    return message.join("\n");
  }
}

export function drawCursors(editor: vscode.TextEditor | undefined) {
  if (editor) {
    let uri = editor.document.uri;
    let allSelections = Array.from(cursors.values())
      .flat()
      .filter((cursor) => cursor.uri.toString() === uri.toString())
      .map((cursor) => cursor.selection);
    editor.setDecorations(selectionDecorationType, allSelections);
  }
}

/// this function is used to follow other users' cursors in the editor. It will be called when the user moves their cursor, and it will send the new cursor position to the daemon.
export async function syncFollowCursor() {
  try {
    if (!useridToFollow) {
      return;
    }
    const usersCursors = cursors.get(useridToFollow);
    if (!usersCursors || usersCursors.length === 0) {
      return;
    }
    const usersCursor = usersCursors[0];
    const editors = vscode.window.visibleTextEditors.filter((editor) => editor.document.uri.toString() === usersCursor.uri.toString());
    if (editors.length === 0) {
      // create editor for the user cursor if it doesn't exist
      const document = await vscode.workspace.openTextDocument(usersCursor.uri);
      editors.push(await vscode.window.showTextDocument(document, { preview: false }));
    }
    const editor = editors[0];
    const selection = usersCursor.selection;
    editor.revealRange(selection.range, vscode.TextEditorRevealType.InCenterIfOutsideViewport);
    editor.selection = new vscode.Selection(selection.range.start, selection.range.end);
  } catch { }
}
export async function followCursor() {

  const items: vscode.QuickPickItem[] = [
    { label: "$(circle-slash) Stop following", description: "__stop__", alwaysShow: true },
  ];

  cursors.forEach((remoteCursors, uid) => {
    if (remoteCursors.length > 0) {
      const label = useridToFollow === uid ? `$(link) ${remoteCursors[0].name}` : remoteCursors[0].name;
      items.push({ label, description: uid });
    }
  });

  const picked = await vscode.window.showQuickPick(items, {
    placeHolder: "Follow which user?",
  });

  if (!picked) { return; }

  if (picked.description === "__stop__") {
    useridToFollow = null;
    return;
  }

  useridToFollow = picked.description || null;
  if (useridToFollow) {
    await syncFollowCursor();
  }
}
