from setuptools import setup

setup(
    name='jsonwatch',
    version='0.1.0',
    description='Track changes in JSON data.',
    url='http://github.com/dbohdan/jsonwatch',
    author='Danyil Bohdan',
    author_email='danyil.bohdan@gmail.com',
    license='MIT',
    packages=['jsonwatch'],
    zip_safe=False,
    entry_points = {
        'console_scripts': [
            'jsonwatch = jsonwatch.jsonwatch:main',
        ],
    }
)
