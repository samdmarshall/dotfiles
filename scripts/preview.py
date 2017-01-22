#!/usr/bin/env python3

import os
import sys
import magic
import argparse

parser = argparse.ArgumentParser(description='generic previewing tool')
parser.add_argument(
    'path',
    metavar='file-path',
    action='store',
)
parser.add_argument(
    '--metadata',
    action='store_true',
)

args = parser.parse_args(sys.argv[1:])

preview_path = os.path.expandvars(os.path.expanduser(args.path))

if os.path.exists(preview_path) is False:
    sys.exit(1)

try:
    if args.metadata is True:
        with magic.Magic() as m:
            print(m.id_filename(preview_path))
    if os.path.islink(preview_path) is True:
        preview_path = os.path.realpath(preview_path)
    if os.path.isdir(preview_path) is True:
        for item in os.listdir(preview_path):
            print(item)
    elif os.path.isfile(preview_path) is True:
        file_size = os.path.getsize(preview_path)
        read_size = min(file_size, 50*(1024*2))
        with open(preview_path, 'r') as fd:
            data = fd.read(read_size)
            print(data)
    elif os.path.ismount(preview_path) is True:
        print('%s is a mount-point' % preview_path)
except:
    print('Unable to access path!')
    sys.exit(1)
