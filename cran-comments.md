
## R CMD check results

0 errors | 0 warnings | 1 note

Fixed broken link from last submission. Replaced `https://www.ine.es/dyngs/SER/en/index.htm?cid=1389` with `https://www.ine.es/en/daco/daco42/codmun/cod_provincia_en.htm`

* The links to www.ine.es and www.transportes.gob.es give false positives with 403 and 400 errors, while in fact they work just fine in the web browser for the end user. It must be some bot protection of the Spanish websites that do not accept HEAD requests.
