## Resubmission
This is a resubmission. In this version:

* R CMD check on winbuilder does not do 'checking examples with --run-donttest', so if you do check with `--run-donttest`, it may fail (e.g. it fails in GitHub actions, but works on my local machine), as downloading of critical xml files that are required for all data downloading functions may fail from some hosts because of the website owner blocking certain requests. To address this, most examples are now conditional on interactive() being TRUE.

* all exported functions have the returned value specified

* all non-exported functions do not have examples

* all examples are that download data are wrapped into donttest instead of dontrun

* `if(FALSE)` removed from all examples


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Regarding the NOTEs of the R CMD check:

  * The links to www.ine.es and www.transportes.gob.es give false positives with 403, while in fact they work just fine in the web browser. It must be some bot protection of the Spanish websites.

  * The DOI gives a warning because it was not yet assinged by CRAN, but we put it into the citation information in advance so that we don't have to update the package after acceptance to CRAN just to put the DOI in.
