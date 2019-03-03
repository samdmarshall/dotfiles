const vscode = require('vscode');
const SvnSpawn = require('svn-spawn');
const path = require('path');

const createStatusBar = () => {
  const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left);
  statusBarItem.command = 'vscode-svn.showOutputChannel';

  return statusBarItem;
};

const createClient = (rootPath) => {
  return new SvnSpawn({
    cwd: rootPath,
    noAuthCache: true,
  });
};

const createChannel = () => {
  return vscode.window.createOutputChannel('vscode-svn');
};

const svnStatus = (client) => {
  return new Promise((resolve, reject) => {
    client.getStatus((err, data) => err ? reject(err) : resolve(data));
  });
};

const updateStatusBar = (data, statusBar) => {
  return new Promise((resolve) => {
    statusBar.text = `${data.length} changes`;
    statusBar.show();

    resolve(data);
  });
};

const checkAllFiles = (client) => {
  return new Promise((resolve, reject) => {
    svnStatus(client)
    .then((data) => resolve(data))
    .catch((err) => reject(err));
  });
};

const updateOutputChannel = (data, outputChannel) => {
  outputChannel.clear();

  data.forEach((item) => {
    const document = vscode.Uri.file(path.join(vscode.workspace.rootPath, item.$.path));
    outputChannel.appendLine(document);
  });
};

exports.activate = () => {
  const rootPath = vscode.workspace.rootPath;
  const outputChannel = createChannel();
  vscode.commands.registerCommand('vscode-svn.showOutputChannel', () => outputChannel.show());

  const statusBar = createStatusBar();
  const client = createClient(rootPath);
  const watcher = vscode.workspace.createFileSystemWatcher(`${rootPath}/**/*`);

  const main = () => {
    return checkAllFiles(client, statusBar)
    .then((data) => updateStatusBar(data, statusBar))
    .then((data) => updateOutputChannel(data, outputChannel))
    .catch((err) => vscode.window.showErrorMessage(err));
  };

  watcher.onDidChange(main);
  watcher.onDidCreate(main);
  watcher.onDidDelete(main);

  main();
};

exports.deactivate = () => {
};
