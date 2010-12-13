#!/usr/bin/env python

import os
import subprocess


def main():
    for dname in os.listdir('.'):
        if not os.path.isdir(dname):
            continue
        command = 'sh %(dname)s.sh' % locals()
        exit_code = subprocess.call(command.split(' '), cwd=dname)

        if not exit_code == 0:
            print '>> Failed running %s' % dname
            return exit_code
    return 0

if __name__ == '__main__':
    exit(main())

