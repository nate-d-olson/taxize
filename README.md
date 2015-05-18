taxize
=======




[![Build Status](https://api.travis-ci.org/ropensci/taxize.png?branch=master)](https://travis-ci.org/ropensci/taxize)
[![Build status](https://ci.appveyor.com/api/projects/status/6mgc02mkd8j4sq3g/branch/master)](https://ci.appveyor.com/project/sckott/taxize-175/branch/master)
[![Coverage Status](https://coveralls.io/repos/ropensci/taxize/badge.svg)](https://coveralls.io/r/ropensci/taxize)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/taxize)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/taxize)](http://cran.rstudio.com/web/packages/taxize)

`taxize` allows users to search over many taxonomic data sources for species names (scientific and common) and download up and downstream taxonomic hierarchical information - among other things.

The `taxize` tutorial is can be found at [http://ropensci.org/tutorials/taxize.html][tut].

The functions in the package that hit a specific API have a prefix and suffix separated by an underscore. They follow the format of `service_whatitdoes`.  For example, `gnr_resolve` uses the Global Names Resolver API to resolve species names.  General functions in the package that don't hit a specific API don't have two words separated by an underscore, e.g., `classification`.

You need API keys for Encyclopedia of Life (EOL), the Universal Biological Indexer and Organizer (uBio), Tropicos, and Plantminer.

## Local data in SQL 

Some data sources have functions in `taxize` that you can use with a local SQL version of the database that you'd otherwise access via a web API. The local SQL version can be advantageous because with requests for large amounts of data, the local version should be much faster. With small data requests, you may not notice much difference, however. The data sources that currently have local SQL support, or will soon have, and some note about each:

<table>
<colgroup>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
</colgroup>

<thead>
<tr>
  <th style="text-align:left;">Source</th>
	<th style="text-align:left;">Database type</th>
	<th style="text-align:left;">Ready to use?</th>
	<th style="text-align:left;">Notes</th>
</tr>
</thead>

<tbody>
<tr>
	<td style="text-align:left;">ITIS</td>
	<td style="text-align:left;">PostgreSQL</td>
	<td style="text-align:left;">Yes</td>
	<td style="text-align:left;">...</td>
</tr>
<tr>
	<td style="text-align:left;">Theplantlist</td>
	<td style="text-align:left;">PostgreSQL</td>
	<td style="text-align:left;">Yes</td>
	<td style="text-align:left;">The Postgres DB is actually made by us from csv files</td>
</tr>
<tr>
	<td style="text-align:left;">COL</td>
	<td style="text-align:left;">MySQL</td>
	<td style="text-align:left;">Yes</td>
	<td style="text-align:left;">We are trying to move to all using PostgreSQL, but for now this is in MySQL</td>
</tr>
<tr>
	<td style="text-align:left;">NCBI</td>
	<td style="text-align:left;">???</td>
	<td style="text-align:left;">No</td>
	<td style="text-align:left;">Dump files need to be arranged manually - we're still working on this</td>
</tr>
</tbody>
</table>

## SOAP

Note that a few data sources require SOAP web services, which are difficult to support in R across all operating systems. These include: World Register of Marine Species, Pan-European Species directories Infrastructure , and Mycobank, so far. Data sources that use SOAP web services have been moved to a new package called `taxizesoap`. Find it at [https://github.com/ropensci/taxizesoap](https://github.com/ropensci/taxizesoap).

## Currently implemented in `taxize`

<table>
<colgroup>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
</colgroup>

<thead>
<tr>
  <th style="text-align:left;">Souce</th>
	<th style="text-align:left;">Function prefix</th>
	<th style="text-align:left;">API Docs</th>
	<th style="text-align:left;">API key</th>
</tr>
</thead>

<tbody>
<tr>
	<td style="text-align:left;">Encylopedia of Life</td>
	<td style="text-align:left;"><code>eol</code></td>
	<td style="text-align:left;"><a href="http://www.eol.org/api/">link</a></td>
	<td style="text-align:left;"><a href="http://eol.org/users/register">link</a></td>
</tr>
<tr>
	<td style="text-align:left;">Taxonomic Name Resolution Service</td>
	<td style="text-align:left;"><code>tnrs</code></td>
	<td style="text-align:left;"><a href="http://api.phylotastic.org/tnrs">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Integrated Taxonomic Information Service</td>
	<td style="text-align:left;"><code>itis</code></td>
	<td style="text-align:left;"><a href="http://www.itis.gov/ws_description.html">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Phylomatic</td>
	<td style="text-align:left;"><code>phylomatic</code></td>
	<td style="text-align:left;"><a href="http://www.phylodiversity.net/phylomatic/phylomatic_api.html">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">uBio</td>
	<td style="text-align:left;"><code>ubio</code></td>
	<td style="text-align:left;"><a href="http://www.ubio.org/index.php?pagename=xml_services">link</a></td>
	<td style="text-align:left;"><a href="http://www.ubio.org/index.php?pagename=form">link</a></td>
</tr>
<tr>
	<td style="text-align:left;">Global Names Resolver</td>
	<td style="text-align:left;"><code>gnr</code></td>
	<td style="text-align:left;"><a href="http://resolver.globalnames.org/api">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Global Names Index</td>
	<td style="text-align:left;"><code>gni</code></td>
	<td style="text-align:left;"><a href="https://github.com/dimus/gni/wiki/api">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">IUCN Red List</td>
	<td style="text-align:left;"><code>iucn</code></td>
	<td style="text-align:left;"><a href="https://www.assembla.com/spaces/sis/wiki/Red_List_API?version=3">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Tropicos</td>
	<td style="text-align:left;"><code>tp</code></td>
	<td style="text-align:left;"><a href="http://services.tropicos.org/help">link</a></td>
	<td style="text-align:left;"><a href="http://services.tropicos.org/help?requestkey">link</a></td>
</tr>
<tr>
	<td style="text-align:left;">Plantminer</td>
	<td style="text-align:left;"><code>plantminer</code></td>
	<td style="text-align:left;"><a href="http://www.plantminer.com/help">link</a></td>
	<td style="text-align:left;"><a href="http://www.plantminer.com/help">link</a></td>
</tr>
<tr>
	<td style="text-align:left;">Theplantlist dot org</td>
	<td style="text-align:left;"><code>tpl</code></td>
	<td style="text-align:left;">**</td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Catalogue of Life</td>
	<td style="text-align:left;"><code>col</code></td>
	<td style="text-align:left;"><a href="http://www.catalogueoflife.org/colwebsite/content/web-services">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Global Invasive Species Database</td>
	<td style="text-align:left;"><code>gisd</code></td>
	<td style="text-align:left;">***</td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">National Center for Biotechnology Information</td>
	<td style="text-align:left;"><code>ncbi</code></td>
	<td style="text-align:left;">none</td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">CANADENSYS Vascan name search API</td>
	<td style="text-align:left;"><code>vascan</code></td>
	<td style="text-align:left;"><a href="http://data.canadensys.net/vascan/api">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">International Plant Names Index (IPNI)</td>
	<td style="text-align:left;"><code>ipni</code></td>
	<td style="text-align:left;"><a href="http://www.ipni.org/link_to_ipni.html">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">Barcode of Life Data Systems (BOLD)</td>
	<td style="text-align:left;"><code>bold</code></td>
	<td style="text-align:left;"><a href="http://www.boldsystems.org/index.php/Resources">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
<tr>
	<td style="text-align:left;">National Biodiversity Network (UK)</td>
	<td style="text-align:left;"><code>nbn</code></td>
	<td style="text-align:left;"><a href="https://data.nbn.org.uk/Documentation/Web_Services/Web_Services-REST/resources/restapi/rest.html">link</a></td>
	<td style="text-align:left;">none</td>
</tr>
</tbody>
</table>

**: There are none! We suggest using `TPL` and `TPLck` functions in the [taxonstand package](http://cran.r-project.org/web/packages/Taxonstand/index.html). We provide two functions to get bullk data: `tpl_families` and `tpl_get`.

\***: There are none! The function scrapes the web directly.

#### May be in taxize in the future...

+ [NatureServe](http://www.natureserve.org/)
+ [Lichen Taxon dictionary](http://www.thebls.org.uk/)
+ [Wikispecies](https://species.wikimedia.org/wiki/Main_Page)
+ And more, See the [newdatasource](https://github.com/ropensci/taxize/labels/newdatasource) tag in the issue tracker

## Quickstart

For more examples see the [tutorial][tut]

### Installation

#### Stable version from CRAN


```r
install.packages("taxize")
```

#### Development version from GitHub

Windows users install [Rtools](http://cran.r-project.org/bin/windows/Rtools/) first.


```r
install.packages("devtools")
devtools::install_github("taxize", "ropensci")
```


```r
library('taxize')
```

### Get unique taxonomic identifier from NCBI

Alot of `taxize` revolves around taxonomic identifiers. Because, as you know, names can be a mess (misspelled, synonyms, etc.), it's better to get an identifier that a particular data sources knows about, then we can move forth acquiring more fun taxonomic data.


```r
uids <- get_uid(c("Chironomus riparius", "Chaetopteryx"))
#> Error in value[[3L]](cond): Empty reply from server
```

### Retrieve classifications

Classifications - think of a species, then all the taxonomic ranks up from that species, like genus, family, order, class, kingdom.


```r
out <- classification(uids)
lapply(out, head)
#> $`315576`
#>                 name         rank     id
#> 1 cellular organisms      no rank 131567
#> 2          Eukaryota superkingdom   2759
#> 3       Opisthokonta      no rank  33154
#> 4            Metazoa      kingdom  33208
#> 5          Eumetazoa      no rank   6072
#> 6          Bilateria      no rank  33213
#> 
#> $`492549`
#>                 name         rank     id
#> 1 cellular organisms      no rank 131567
#> 2          Eukaryota superkingdom   2759
#> 3       Opisthokonta      no rank  33154
#> 4            Metazoa      kingdom  33208
#> 5          Eumetazoa      no rank   6072
#> 6          Bilateria      no rank  33213
```

### Immediate children

Get immediate children of _Salmo_. In this case, _Salmo_ is a genus, so this gives species within the genus.


```r
children("Salmo", db = 'ncbi')
#> Error in function (type, msg, asError = TRUE) : Empty reply from server
```

### Downstream children to a rank

Get all species in the genus _Apis_


```r
downstream("Apis", db = 'itis', downto = 'Species', verbose = FALSE)
#> Error in function (type, msg, asError = TRUE) : Empty reply from server
```

### Upstream taxa

Get all genera up from the species _Pinus contorta_ (this includes the genus of the species, and its co-genera within the same family).


```r
upstream("Pinus contorta", db = 'itis', upto = 'Genus', verbose=FALSE)
#> Error in function (type, msg, asError = TRUE) : Empty reply from server
```

### Get synonyms


```r
synonyms("Salmo friderici", db='ubio')
#>   namebankid          target family rankname
#> 1    2529704 Salmo friderici Pisces  species
#> 2     169693 Salmo friderici Pisces  species
#> $`Salmo friderici`
#>   namebankid                    namestring
#> 1     130562 Leporinus friderici friderici
#> 2     169693               Salmo friderici
#> 3    2495407 Leporinus friderici friderici
#>                                fullnamestring
#> 1 Leporinus friderici friderici (Bloch, 1794)
#> 2                 Salmo friderici Bloch, 1794
#> 3               Leporinus friderici friderici
```

### Get taxonomic IDs from many sources


```r
get_ids(names="Salvelinus fontinalis", db = c('ubio','ncbi'), verbose=FALSE)
#>   namebankid                target     family  rankname
#> 1    2501330 Salvelinus fontinalis     Pisces   species
#> 2    6581534 Salvelinus fontinalis Salmonidae   species
#> 3     137827 Salvelinus fontinalis     Pisces   species
#> 4    6244425 Salvelinus fontinalis Salmonidae trinomial
#> 5    7130714 Salvelinus fontinalis Salmonidae trinomial
#> 6    6653671 Salvelinus fontinalis Salmonidae trinomial
#> Error in value[[3L]](cond): Empty reply from server
```

You can limit to certain rows when getting ids in any `get_*()` functions


```r
get_ids(names="Poa annua", db = "gbif", rows=1)
#> $gbif
#> Poa annua 
#> "2704179" 
#> attr(,"class")
#> [1] "gbifid"
#> attr(,"match")
#> [1] "found"
#> attr(,"uri")
#> [1] "http://www.gbif.org/species/2704179"
#> 
#> attr(,"class")
#> [1] "ids"
```

Furthermore, you can just back all ids if that's your jam with the `get_*_()` functions (all `get_*()` functions with additional `_` underscore at end of function name)


```r
get_ids_(c("Chironomus riparius", "Pinus contorta"), db = 'nbn', rows=1:3)
<<<<<<< HEAD
#> Error in function (type, msg, asError = TRUE) : Server aborted the SSL handshake
=======
#> $nbn
#> $nbn$`Chironomus riparius`
#>   ptaxonVersionKey    searchMatchTitle    rank  nameStatus
#> 1 NBNSYS0000027573 Chironomus riparius Species Recommended
#> 2 NBNSYS0000023345   Paederus riparius Species Recommended
#> 3 NHMSYS0001718042   Elaphrus riparius Species Recommended
#> 
#> $nbn$`Pinus contorta`
#>   ptaxonVersionKey               searchMatchTitle       rank  nameStatus
#> 1 NHMSYS0000494848   Pinus contorta var. contorta    Variety Recommended
#> 2 NBNSYS0000004786                 Pinus contorta    Species Recommended
#> 3 NHMSYS0000494848 Pinus contorta subsp. contorta Subspecies Recommended
#> 
#> 
#> attr(,"class")
#> [1] "ids"
>>>>>>> master
```

### Common names from scientific names


```r
<<<<<<< HEAD
sci2comm('Helianthus annuus', db='itis')
#> Error in function (type, msg, asError = TRUE) : Empty reply from server
=======
sci2comm('Helianthus annuus', db = 'itis')
#> $`Helianthus annuus`
#> [1] "common sunflower" "sunflower"        "wild sunflower"  
#> [4] "annual sunflower"
>>>>>>> master
```

### Scientific names from common names


```r
comm2sci("black bear", db = "itis")
#> $`black bear`
<<<<<<< HEAD
<<<<<<< HEAD
#>  [1] "Ursus americanus Pallas, 1780"              
<<<<<<< HEAD
#>  [2] "Ursus americanus americanus Pallas, 1780"   
#>  [3] "Ursus thibetanus G. [Baron] Cuvier, 1823"   
#>  [4] "Ursus americanus floridanus Merriam, 1896"  
#>  [5] "Ursus americanus luteolus Griffith, 1821"   
#>  [6] "Ursus thibetanus formosanus Swinhoe, 1864"  
#>  [7] "Ursus americanus kermodei Hornaday, 1905"   
#>  [8] "Ursus americanus eremicus Merriam, 1904"    
#>  [9] "Ursus americanus perniger J. A. Allen, 1910"
#> [10] "Ursus thibetanus ussuricus (Heude, 1901)"   
#> [11] "Ursus thibetanus japonicus Schlegel, 1857"  
#> [12] "Prosimulium ursinum (Edwards, 1935)"        
=======
#>  [2] "Ursus americanus floridanus Merriam, 1896"  
#>  [3] "Ursus americanus luteolus Griffith, 1821"   
#>  [4] "Ursus thibetanus formosanus Swinhoe, 1864"  
#>  [5] "Ursus americanus americanus Pallas, 1780"   
#>  [6] "Ursus americanus kermodei Hornaday, 1905"   
#>  [7] "Ursus americanus eremicus Merriam, 1904"    
#>  [8] "Ursus americanus perniger J. A. Allen, 1910"
#>  [9] "Ursus thibetanus ussuricus (Heude, 1901)"   
#> [10] "Ursus thibetanus japonicus Schlegel, 1857"  
#> [11] "Prosimulium ursinum (Edwards, 1935)"        
#> [12] "Ursus thibetanus G. [Baron] Cuvier, 1823"   
>>>>>>> master
#> [13] "Pyrrharctia isabella Smith 1797"
=======
#> [1] "Chiropotes satanas"          "Ursus thibetanus"           
#> [3] "Ursus thibetanus"            "Ursus americanus luteolus"  
#> [5] "Ursus americanus americanus" "Ursus americanus"           
#> [7] "Ursus americanus"
>>>>>>> master
=======
#> [1] "Chiropotes satanas"          "Ursus americanus luteolus"  
#> [3] "Ursus americanus americanus" "Ursus americanus"           
#> [5] "Ursus americanus"            "Ursus thibetanus"           
#> [7] "Ursus thibetanus"
>>>>>>> master
```

### Coerce codes to taxonomic id classes

`numeric` to `uid`


```r
as.uid(315567)
#> Error in function (type, msg, asError = TRUE) : Empty reply from server
```

`list` to `uid`


```r
<<<<<<< HEAD
as.uid(list("315567","3339","9696"))
#> Error in function (type, msg, asError = TRUE) : Empty reply from server
=======
as.uid(list("315567", "3339", "9696"))
#> [1] "315567" "3339"   "9696"  
#> attr(,"class")
#> [1] "uid"
#> attr(,"match")
#> [1] "found" "found" "found"
#> attr(,"uri")
#> [1] "http://www.ncbi.nlm.nih.gov/taxonomy/315567"
#> [2] "http://www.ncbi.nlm.nih.gov/taxonomy/3339"  
#> [3] "http://www.ncbi.nlm.nih.gov/taxonomy/9696"
>>>>>>> master
```

### Coerce taxonomic id classes to a data.frame


```r
<<<<<<< HEAD
out <- as.uid(c(315567,3339,9696))
#> Error in function (type, msg, asError = TRUE) : Empty reply from server
=======
out <- as.uid(c(315567, 3339, 9696))
>>>>>>> master
(res <- data.frame(out))
#> Error in data.frame(out): object 'out' not found
```

## Contributors

+ [Scott Chamberlain](https://github.com/sckott)
+ [Eduard Szöcs](https://github.com/EDiLD)
+ [Zachary Foster](https://github.com/zachary-foster)
+ [Carl Boettiger](https://github.com/cboettig)
+ [Karthik Ram](https://github.com/karthik)
+ [Ignasi Bartomeus](https://github.com/ibartomeus)
+ [John Baumgartner](https://github.com/johnbaums)

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/taxize/issues).
* License: MIT
* Get citation information for `taxize` in R doing `citation(package = 'taxize')`

[![ropensci](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)

[tut]: http://ropensci.org/tutorials/taxize.html
