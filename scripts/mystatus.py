#!/usr/bin/env python3

import os
import sys
import json
import subprocess
import portalocker

try:
    from subprocess import DEVNULL
except ImportError: # pragma: no cover
    DEVNULL = open(os.devnull, 'wb')

def Invoke(call_args, shell_state=False):
    error = 0
    output = None
    try:
        output = subprocess.check_output(call_args, shell=shell_state, stderr=DEVNULL).decode(sys.stdout.encoding)
    except subprocess.CalledProcessError as exception:
        output = exception.output.decode(sys.stdout.encoding)
        error = exception.returncode
    return (output, error)

class Settings(object):
    def __init__(self):
        self.plugins_path = os.path.expanduser('~/.config/mystatus/config')
        fd = open(self.plugins_path)
        data = json.load(fd)
        fd.close()
        self.plugins = data.get('plugins')

    def run(self, plugin_data):
        status = False
        environment = plugin_data.get('env')
        for key in list(environment.keys()):
            os.environ[key] = environment.get(key)
        result = Invoke(plugin_data.get("command",[]))
        if len(result[0]) > 0:
            status = len(result[0].split('\n')) > 0
        for key in list(environment.keys()):
            os.unsetenv(key)
        return status

def main(argv=sys.argv[1:]):
    lockfile_path = os.path.expanduser('~/.config/mystatus/lockfile')
    lock_fd = portalocker.Lock(lockfile_path, mode='r+')
    if 'status' in argv:
        has_status = False
        data = json.load(lock_fd.acquire())
        for key in list(data.keys()):
            has_status = data.get(key)
            if has_status is True:
                break
        if has_status is True:
            print('!')
    else:
        data = {}
        prefs = Settings()
        for plugin_data in prefs.plugins:
            name = plugin_data.get('name')
            result = prefs.run(plugin_data)
            data[name] = result
        json.dump(data, lock_fd.acquire())
    lock_fd.release()
    sys.exit(0)
    

if __name__ == '__main__':
    main()
