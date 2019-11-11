"""
Configuration data
"""
import os


db_path = os.path.join(os.path.dirname(__file__), 'content.sqlite')

ignore_users = [
    ''
]

ignore_dirs = [
    '.Rproj.user',
]

