from ast import literal_eval
from setuptools import setup

# Get package information, including version, from jsonwatch/__init__.py.
with open("jsonwatch/__init__.py") as f:
    lns = [s.strip().split(' = ') for s in f.readlines()]
    # Safely evaluate right-hand side values and put package info into a dict.
    package_info = dict([(elem[0], literal_eval(elem[1])) for elem in lns
                         if len(elem) == 2])

# Get requirements.
with open('requirements.txt') as f:
    requirements = f.read().splitlines()

setup(
    name='jsonwatch',
    version=package_info['__version__'],
    description='Track changes in JSON data.',
    url='http://github.com/dbohdan/jsonwatch',
    author='Danyil Bohdan',
    author_email='danyil.bohdan@gmail.com',
    license='MIT',
    packages=['jsonwatch'],
    package_dir='',
    data_files=[('', ['LICENSE', 'README.md'])],
    test_suite='jsonwatch.tests.suite',
    zip_safe=False,
    install_requires=requirements,
    entry_points={
        'console_scripts': [
            'jsonwatch = jsonwatch.jsonwatch:main',
        ],
    }
)
