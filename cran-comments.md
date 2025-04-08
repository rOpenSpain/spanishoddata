
## R CMD check results

0 errors | 0 warnings | 1 note

* Introduced the dependency on R >= 4.1.0 to address the NOTE: "Missing dependency on R >= 4.1.0 because package code uses the pipe"

* The links to www.ine.es and www.transportes.gob.es give false positives with 403, while in fact they work just fine in the web browser. It must be some bot protection of the Spanish websites.
