import os
import sys
import argparse
import string
import subprocess
from subprocess import CalledProcessError
import tempfile
import stat
import paramiko
from paramiko import SSHClient
from scp import SCPClient
import shutil

update_identifier = '*'
WEBSITE_ROOT='/var/www/pewpewthespells.com/public_html/'
HOST_NAME='pewpewthespells.com'
USER_NAME='samdm'

REMOVE_JS_SCRIPT = "#!/usr/bin/env bash\n\
# script used for stripping the js file out for IE\n\
lineone=`sed '/\<!--\[if lt IE 9\]\>/d' $1`\n\
linetwo=`echo \"$lineone\" | sed '/\<script src=\"http\:\/\/html5shim\.googlecode\.com\/svn\/trunk\/html5.js\"\>\<\/script\>/d'`\n\
linethree=`echo \"$linetwo\" | sed '/\<!\[endif\]--\>/d'`\n\
linefour=`echo \"$linethree\" | sed '/\<style type=\"text\/css\"\>code{white-space\: pre;}\<\/style\>/d'`\n\
linefive=`echo \"$linefour\" | sed '/\<style type=\"text\/css\"\>\<\/style\>/d'`\n\
echo \"$linefive\" > $1"

def make_subprocess_call(call_args):
    error = 0
    output = ''
    try:
        output = subprocess.check_output(call_args)
        error = 0
    except CalledProcessError as e:
        output = e.output
        error = e.returncode
    return (output, error)

def ExportHTML(site_path, item_path, export_dir):
    HEADER_PATH = os.path.join(site_path, 'header.html')
    INPUT_FILE_PATH = os.path.join(site_path, item_path)
    # convert extension to html
    item_name = os.path.splitext(os.path.basename(item_path))[0]
    item_dir = os.path.dirname(item_path)
    html_path = os.path.join(item_dir, item_name+'.html')
    EXPORT_FILE_PATH = os.path.join(export_dir, html_path)
    make_subprocess_call(('pandoc', '-f', 'markdown+grid_tables', '-t', 'html5', '-H', HEADER_PATH, '--email-obfuscation', 'references', INPUT_FILE_PATH, '-o', EXPORT_FILE_PATH))
    fd = tempfile.NamedTemporaryFile(delete=False)
    fd.write(REMOVE_JS_SCRIPT)
    fd.close()
    st = os.stat(fd.name)
    os.chmod(fd.name, st.st_mode | stat.S_IEXEC)
    make_subprocess_call((fd.name, EXPORT_FILE_PATH))
    os.unlink(fd.name)
    return EXPORT_FILE_PATH

def ExportPDF(site_path, item_path, export_dir):
    # convert extension to html
    item_name = os.path.splitext(os.path.basename(item_path))[0]
    item_dir = os.path.dirname(item_path)
    html_path = os.path.join(item_dir, item_name+'.html')
    pdf_path = os.path.join(item_dir, item_name+'.pdf')
    INPUT_FILE_PATH = os.path.join(export_dir, html_path)
    EXPORT_DIR = export_dir
    make_subprocess_call(('html2pdf', '-b', EXPORT_DIR, '-i', INPUT_FILE_PATH))
    EXPORT_FILE_PATH = os.path.join(export_dir, pdf_path)
    return EXPORT_FILE_PATH

export_rules = {
  'html': ExportHTML,
  'pdf': ExportPDF,
}

def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('site_map_path', help='path to sitemap.txt')
    parser.add_argument('-p', '--preview', action='store_true')
    parser.add_argument('-u', '--upload', action='store_true')
    
    args = vars(parser.parse_args())
    
    export_items = list()
    
    site_map_path = os.path.abspath(args.get('site_map_path'))
    site_path = os.path.dirname(site_map_path)
    export_path = os.path.join(os.path.dirname(site_path), '_export')
    if os.path.exists(export_path) == False:
        os.mkdir(export_path)
    
    if args.get('preview') or args.get('upload'):
        with open(site_map_path, 'rw') as fd:
            for line in fd:
                line = line.rstrip('\n')
                # skip lines that are empty or start with a comment
                if len(line) == 0 or line.startswith('#'):
                    continue
                
                # parse all other lines, format should be as follows:
                # path/to/file.extension_on_disk comma,separated,extensions,to,create symbol_here_to_indicate_file_should_be_updated
                item_path, file_ext, update = (line.split(' ') + list(' '))[:3]
            
                full_path = os.path.join(site_path, item_path)
            
                update = update.strip(' ')
                
                # if the 3rd field has a value, then we want to update the page
                if len(update) > 0:
                    if update == update_identifier:
                        for extension in file_ext.split(','):
                            print('Processing: %s -> %s' % (full_path, extension))
                            
                            # create the directory structure in the export directory so that 
                            export_file_path_dirs = item_path.split(os.sep)[:1]
                            item_counter = 1
                            for component in export_file_path_dirs:
                                component_name, component_extension = os.path.splitext(component)
                                if component_extension == '':
                                    partial_path = string.join(export_file_path_dirs[0:item_counter], os.sep)
                                    export_item_dir_path = os.path.normpath(os.path.join(export_path, partial_path))
                                    if os.path.exists(export_item_dir_path) == False:
                                        os.mkdir(export_item_dir_path)
                                item_counter = item_counter + 1
                            # check to see if we need to perform an export action or copy the file
                            item_extension = os.path.splitext(os.path.basename(full_path))[1].replace('.', '')
                            if item_extension == extension:
                                # same file extension, so copy it over
                                export_file_path = os.path.join(export_path, item_path)
                                shutil.copy2(full_path, export_file_path)
                                export_items.append(export_file_path)
                                continue
                        
                            # otherwise we need to process the extension for a rule
                            if extension in export_rules.keys():
                                function = export_rules[extension]
                                exported_item_path = function(site_path, item_path, export_path)
                                export_items.append(exported_item_path)
                            else:
                                print('Error: No rule found for export extension "%s" for file "%s"' % (extension, item_path))
                                sys.exit()
                    else:
                        print('Error: Unknown identifier found in sitemap file! Please use "%s" to indicate an update' % update_identifier)
                        sys.exit()
    # now upload if necessary
    if args.get('upload'):
        ssh = paramiko.SSHClient();
        ssh.load_system_host_keys();
        ssh.connect(HOST_NAME, username=USER_NAME);
        scp = SCPClient(ssh.get_transport());
        for item in export_items:
            item_path = os.path.relpath(item, export_path)
            remote_path = os.path.normpath(os.path.join(WEBSITE_ROOT, item_path))
            item_dirs = item_path.split(os.sep)[1:]
            item_counter = 1
            for component in item_dirs:
                component_name, component_extension = os.path.splitext(component)
                if component_extension == '':
                    partial_path = string.join(item_dirs[0:item_counter], os.sep)
                    ssh.exec_command('mkdir -p '+os.path.normpath(os.path.join(WEBSITE_ROOT, partial_path)))
                item_counter = item_counter + 1
            print('Uploading: %s -> %s@%s:%s' % (item, USER_NAME, HOST_NAME, remote_path))
            scp.put(item, remote_path);
        ssh.close();
        # remove local files that were uploaded
        shutil.rmtree(export_path)

if __name__ == "__main__":
    main(sys.argv[1:])
