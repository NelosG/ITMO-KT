import setuptools
from setuptools import setup

with open("README.md", "r") as fh:
    long_description = fh.read()

setup(
    name='da2package',
    version='0.0.4-alpha',
    description='A example Python package',
    long_description=long_description,
    url='https://github.com/NelosG/da2package',
    author='Gleb Pushkarev',
    author_email='gleb.pushkarev@gmail.com',
    license='License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
    package_dir={"": "src"},
    packages=setuptools.find_packages(where="src"),
    python_requires=">=3.6",
    install_requires=[
        'numpy>=1.16',
        'matplotlib',
    ],

    classifiers=[
        'Development Status :: 1 - Planning',
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: OS Independent",
    ],
)