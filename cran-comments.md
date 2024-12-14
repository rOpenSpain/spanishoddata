## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Regarding the NOTEs of the R CMD check:

  * The links to www.ine.es and www.transportes.gob.es give false positives with 403, while in fact they work just fine in the web browser. It must be some bot protection of the Spanish websites.

  * The DOI gives a warning because it was not yet assinged by CRAN, but we put it into the citation information in advance so that we don't have to update the package after acceptance to CRAN just to put the DOI in.
