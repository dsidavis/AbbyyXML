library(AbbyyXML)
f = system.file("sampleDocs", "UCD_Lehmann_2725.xml", package = "AbbyyXML")
doc = readAbbyy(f)
doc = as(f, "AbbyyXMLDocument")
p = doc[[1]]
stopifnot(is(p, "AbbyyXMLPage"))

plot(p, cex = .5)

bb = getBBox(p)
bb2 = getBBox(XML::getNodeSet(p, ".//x:line", "x"))
stopifnot(identical(bb, bb2))



