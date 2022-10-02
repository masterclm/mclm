This is a resubmission after fixing two URLs, the Description and the use of \dontrun{}.
Instead, examples where files are saved and read follow the {readr} examples,
changing working directory to `tempdir()` inside a \dontshow{} and then changing it back.

## R CMD check results

### Locally

There were no ERRORs, WARNINGs or NOTEs.

### R-hub checks

There were no ERRORs or WARNINGs.
There were two NOTEs in all distributions except Debian:

- Maintainer: 'Mariana Montes <mariana.montes@kuleuven.be>'
New submission

Possibly misspelled words in DESCRIPTION:
  tokenize (14:48)
  variationist (16:25)
These words are technicisms and they are correctly spelled.

Additionally, one NOTE for the Windows build:

- Found the following files/directories:
  'lastMiKTeXException'
  
And two NOTEs in the Fedora build:

- Skipping checking HTML validation: no command 'tidy' found
- Skipping checking math rendering: package 'V8' unavailable

### win_devel

Status: No ERRORs or WARNINGs, 1 NOTE
R Under development (unstable) (2022-09-22 r82895 ucrt)

## Downstream dependencies (revdepcheck results)

There are no downstream dependencies.