#! /usr/bin/python3

from setuptools import find_packages, setup

setup(\
        name="plotReportLib",\
        packages=find_packages(include=["plotReportLib"]),\
        version="0.1.0", \
        description=" first Python library", \
        author="Jeff Severino", \
        license="UToledo",\
        install_requires=['pandas'],\
        setup_requires=['pytest-runner'],\
        tests_require=['pytest==4.4.1'],\
        test_suite='tests',\
        )

