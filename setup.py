from setuptools import setup

execfile('src/jsonwatch/__init__.py')

setup(
    name='jsonwatch',
    version=__version__,
    description='Track changes in JSON data.',
    url='http://github.com/dbohdan/jsonwatch',
    author='Danyil Bohdan',
    author_email='danyil.bohdan@gmail.com',
    license='MIT',
    packages=['jsonwatch'],
    package_dir = {'': 'src'},
    data_files=['LICENSE', 'README.md'],
    test_suite='jsonwatch.tests.suite',
    zip_safe=False,
    entry_points = {
        'console_scripts': [
            'jsonwatch = jsonwatch.jsonwatch:main',
        ],
    }
)
