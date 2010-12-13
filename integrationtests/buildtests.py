#!/usr/bin/env python

import os
import subprocess


def main():
    for dname in os.listdir('.'):
        if not os.path.isdir(dname):
            continue
        command = 'gprbuild -p %(dname)s.gpr' % locals()
        exit_code = subprocess.call(command.split(' '), cwd=dname)

        if not exit_code == 0:
            print '>> Failed building %s' % dname
            return exit_code
    return 0

if __name__ == '__main__':
    exit(main())

