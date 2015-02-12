import os
import shutil
import time
import string
import sys
import subprocess
from subprocess import CalledProcessError
import paramiko
from paramiko import SSHClient
from scp import SCPClient
# Globals
SITE_PATH='';
EXPORT_PATH='';
FILE_BLACKLIST=['.DS_Store', '.git', '.gitignore'];
EXPORT_LIST=[];
WEBSITE_ROOT='/var/www/samdmarshall.com/public_html/';
HOST_NAME='samdmarshall.com';
USER_NAME='samdm';
CSS_URL='/shiny/styling.css';
REMOVE_JS_SCRIPT=os.path.join(os.path.abspath(os.path.dirname(__file__)), 'remove-js.sh');
# Helper Functions
def MakeDirectory(path):
    if PathExists(path) == False:
        os.mkdir(path);
def MakeSymLink(original, path):
    if PathExists(path) == False:
        os.symlink(original, path);
def PathExists(path):
    return os.path.exists(path);
def make_subprocess_call(call_args):
    error = 0;
    output = '';
    try:
        output = subprocess.check_output(call_args);
        error = 0;
    except CalledProcessError as e:
        output = e.output;
        error = e.returncode;
    return (output, error);
def should_update(installed, update):
    if PathExists(installed) == False:
        return True;
    existing = os.path.getmtime(installed);
    updated = os.path.getmtime(update);
    return existing < updated;
def test_for_in_git(check_path):
    result = False;
    check_name = check_path;
    split_path = os.path.split(check_path);
    while len(split_path[0]) != 0:
        split_path = os.path.split(split_path[0]);
    if split_path[1] == '.git':
        result = True;
    
    return result;
# Main
def main(argv):
    if len(argv) == 0:
        print 'Root of markdown site not given!';
        sys.exit();
    elif len(argv) == 1:
        SITE_PATH = argv[0];
        EXPORT_PATH = os.path.join(os.path.dirname(os.path.dirname(SITE_PATH)), 'export');
    else:
        print 'Too many arguments!';
        sys.exit();
    
    if SITE_PATH == '' or EXPORT_PATH == '':
        print 'Could not get site and export paths!';
        sys.exit();
    
    print 'Site: ' + SITE_PATH;
    print 'Export: ' + EXPORT_PATH;
    
    if PathExists(SITE_PATH) == True:
        MakeDirectory(EXPORT_PATH);
        for root, dirs, files in os.walk(SITE_PATH, followlinks=False):
            for dir_name in dirs:
                if test_for_in_git(root.split(SITE_PATH)[1]) == False and dir_name != '.git':
                    export_dir_path = os.path.join(os.path.join(EXPORT_PATH, root.split(SITE_PATH)[1]), dir_name);
                    MakeDirectory(export_dir_path);
            for file_name in files:
                if test_for_in_git(root.split(SITE_PATH)[1]) == False:
                    site_file_path = os.path.join(root, file_name);
                    file_basename, file_extension = os.path.splitext(file_name);
                    if file_extension == '.md':
                        file_name = file_basename+'.html'
                    export_file_path = os.path.join(os.path.join(EXPORT_PATH, root.split(SITE_PATH)[1]), file_name);
                    if file_name not in FILE_BLACKLIST:
                        if should_update(export_file_path, site_file_path):
                            print 'Exporting \"'+os.path.normpath(root.split(SITE_PATH)[1]+'/'+file_name)+'\" ...';
                            EXPORT_LIST.append(export_file_path);
                            if file_extension == '.md':
                                make_subprocess_call(('pandoc', '-f', 'markdown', '-t', 'html5', '-c', CSS_URL, site_file_path, '-o', export_file_path));
                                make_subprocess_call((REMOVE_JS_SCRIPT, export_file_path));
                            else:
                                shutil.copy2(site_file_path, export_file_path);
    ssh = paramiko.SSHClient();
    ssh.load_system_host_keys();
    ssh.connect(HOST_NAME, username=USER_NAME);
    scp = SCPClient(ssh.get_transport());
    for item in EXPORT_LIST:
        item_path = item.split(EXPORT_PATH)[1];
        remote_path = os.path.normpath(WEBSITE_ROOT + item_path);
        item_dirs = item_path.split(os.sep)[1:];
        item_counter = 1;
        for component in item_dirs:
            component_name, component_extension = os.path.splitext(component);
            if component_extension == '':
                partial_path = string.join(item_dirs[0:item_counter],os.sep);
                ssh.exec_command('mkdir -p '+os.path.normpath(WEBSITE_ROOT + partial_path));
            item_counter = item_counter + 1;
        print 'Uploading \"'+item_path+'\" ...'
        scp.put(item, remote_path);
    ssh.close();
if __name__ == "__main__":
    main(sys.argv[1:]);