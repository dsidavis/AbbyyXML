readAbbyy =
function(file, ...)
{
    ans = xmlParse(file, ...)
    class(ans) = c("AbbyyXMLDocument", class(ans))
    ans
}    

setOldClass(c("AbbyyXMLDocument", "XMLInternalDocument", "XMLAbstractDocument"))
setAs("character", "AbbyyXMLDocument", function(from) readAbbyy(from))  # different parameter name would give a warning?

# Would like to inherit from the ReadPDF package for page[[1]]

setOldClass(c("AbbyyXMLPage", "XMLInternalElementNode", "XMLInternalNode", "XMLAbstractNode"))

tmp = "[[.AbbyyXMLDocument" =
          function(x, i, ...) {
              ans = getNodeSet(x, "//x:page", "x")[[i]]
              class(ans) = c("AbbyyXMLPage", "XMLInternalElementNode", "XMLInternalNode", "XMLAbstractNode")
              ans
          }    

setMethod("[[", c("AbbyyXMLDocument", "numeric"), tmp)


