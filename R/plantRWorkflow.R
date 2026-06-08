
plantRWorkflow <- function(x, subsetToProvince = FALSE) {
    # Standardize missing information
    x[x==""] <- NA

    # # Subset country
    # print(dim(x))
    # x <- subset(x, is.na(country) | grepl("br", tolower(country), fixed=T))

    # Lets format this
    print("Formatting occs")
    x <- formatOcc(x, noNumb = NA, noYear = NA, noName = NA)

    print("Formatting locs")
    if(!exists("COUNTRY")) COUNTRY <- "Brazil"
    if(!exists("STATEPROVINCE")) STATEPROVINCE <- "São Paulo"
    x <- fixLocation(x)

    if(subsetToProvince) {
        print("Subsetting to country...")

        tab(x$country.correct)
        tab(x$country.new[is.na(x$country.correct)])
        x <- subset(x, country.correct == COUNTRY)

        # noCountry <- subset(dt, is.na(country.correct))
        tab(x$stateProvince.correct)
        tab(x$municipality.new[is.na(x$stateProvince.correct)])
        print("Subsetting to state...")
        x <- subset(x, stateProvince.correct == STATEPROVINCE | is.na(stateProvince.correct))
    }

    # Treat gps data
    print("Formatting coords...")
    x <- formatCoord(x)

    # formatTax and validateTax
    print("Formatting taxonomy...")
    x <- getTaxonId(x)

    # We'll try getting extra taxons with wfo
    # loading the WFO and WCVP backbones into a temporary environment
    data(list = c("wfoNames", "wcvpNames"), package = "plantRdata")
    # using the World Flora Online
    # x <- tryAgain(x, not_found, getTaxonId, db = wfoNames)
    # using the World Checklist of Vascular Plants
    # x <- tryAgain(x, not_found, getTaxonId, db = wcvpNames)

    # Save unmatched taxons
    nf <- x[x$tax.notes == "not found" | !startsWith(x$id, "bfo"), ]
    nf <- aggregate(nf$catalogNumber, list(family=nf$family, scientificName=nf$scientificName, scientificNameAuthorship=nf$scientificNameAuthorship, id=nf$id), function(x) length(unique(x)))
    nf <- nf[order(nf$family, nf$scientificName),]
    write.csv(nf[nf$x>=10,], "results/taxons_not_found.csv", row.names=F)

    # validate
    print("Validating location info...")
    x <- validateLoc(x)

    print("Validating identification info...")
    # validate taxonomist
    x <- validateTax(x, generalist = T)
    x$tax.check <- factor(x$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)


    print("Validating geolocation info...")
    map <- latamMap$brazil
    map <- subset(map, NAME_1 == "sao paulo")
    x <- validateCoord(x, high.map = map) # WORKING
    x <- tryAgain(x, function(x) is.na(x$decimalLatitude.new), formatCoord)
    x <- tryAgain(x, function(x) is.na(x$geo.check), validateCoord, high.map=map)
    tab(is.na(x$geo.check))
    table(x$geo.check, x$origin.coord)

    x
}