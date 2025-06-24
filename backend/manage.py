#!/usr/bin/env python
"""Django's command-line utility for administrative tasks."""
import os, sys


sys.path.insert(0, os.path.dirname(__file__))

if __name__ == '__main__':
    os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'config.settings')
    from django.core.management import execute_from_command_line
    execute_from_command_line(sys.argv)
