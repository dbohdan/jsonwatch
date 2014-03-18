from setuptools import setup

setup(
    name='jsonwatch',
    version='0.1',
    description='Track changes in JSON data.',
    url='http://github.com/dbohdan/jsonwatch',
    author='Danyil Bohdan',
    author_email='danyil.bohdan@gmail.com',
    license='MIT',
    packages=['jsonwatch'],
    package_dir = {'': 'src'},
    test_suite='jsonwatch.tests.suite',
    zip_safe=False,
    entry_points = {
        'console_scripts': [
            'jsonwatch = jsonwatch.jsonwatch:main',
        ],
    }
)
