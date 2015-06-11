# -*- coding: utf-8 -*-
# Inits
from distutils.core import setup


# Infos
version = '0.1'
description = 'Fortran static source code analysis'
long_description = "Fortran (2003) statis source code analysis for more secured and bug free applications"
author = 'Ahmed Ratnani'
author_email = 'ratnaniahmed@gmail.com'
maintainer = "Ahmed Ratnani"
maintainer_email = "ratnaniahmed@gmail.com"
plateform = 'Linux/UNIX/BSD'
license="MIT"
url="https://github.com/ratnania/fortlint"
classifiers = ["Development Status :: 4 - Beta",
               "Intended Audience :: Science/Research",
               "License :: MIT",
               "Programming Language :: Python :: 2",
               "Programming Language :: Python :: 3",
               "Topic :: Scientific/Engineering :: Physics",
               "Topic :: Scientific/Engineering :: Mathematics",
               "Topic :: Software Development :: Libraries :: Python Modules",
               "Operating System :: POSIX",
               "Operating System :: UNIX",
               "Operating System :: MacOS :: MacOS X",
               ]

packages=[  'fortlint' \
         ]
package_dir={  'fortlint': 'fortlint'\
              ,}



if __name__ == '__main__':

    # Lauch setup
    setup(name='fortlint',
        version = version,
        description = description,
        long_description = long_description,
        author = author,
        author_email = author_email,
        maintainer = author,
        maintainer_email = author_email,
        license = license,
        url=url,
        classifiers = classifiers,
        packages = packages,
        package_dir = package_dir

    )



