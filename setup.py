#!/usr/bin/env python2

try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

setup(
    name='humblexmlparse',
    version='0.0',

    url='',
    description='a variant of simplexmlparse',

    classifiers = [
        "Development Status :: 1 - Planning",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: GNU Affero General Public License v3",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 2.7",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Topic :: Text Processing :: Markup :: XML",
    ],

    author='bhuztez',
    author_email='bhuztez@gmail.com',

    py_modules=['humblexmlparse'],

    zip_safe = False,
)


