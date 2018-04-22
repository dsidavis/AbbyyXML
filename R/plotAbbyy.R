# <separator>  lines   start and end nodes, thickness attribute


plotPage =
    #
    # Can drop the text that is in pictures.
    #
    # showPicText needs to figure out where the picture blocks are and find the intersecting text
    #
    #
function(p, dims = getPageDims(p), title = basename(docName(p)), showSeparators = FALSE, showPicText = TRUE)
{
    plot(0, type = "n", xlab = "", ylab = "", xlim = c(0, dims["width"]), ylim = c(0, dims["height"]))
    title(title)
    
    h = dims["height"]

    pics = getNodeSet(p, ".//x:block[@blockType = 'Picture' or @blockType = 'Table']", "x")
    lines = getNodeSet(p, ".//x:line", "x")

#   pics = getNodeSet(p, ".//x:block[@blockType = 'Table']", "x")
#   lines = getNodeSet(p, ".//x:block[not(@blockType = 'Picture')]//x:line", "x") # XXX doesn't make sense. Not how it is formatted.

    
    if(length(pics)) {
        bb = getBBox(pics)
        rect(bb[, "left"], h - bb[, "bottom"], bb[, "right"], h - bb[, "top"], border = c(Picture = "lightgreen", Table = "red")[sapply(pics, xmlGetAttr, "blockType")])
    }

    if(showSeparators) {
        #XXX use the thickness and dotted styles
        sep = getSeparators(getNodeSet(p, ".//x:separator", "x"))
        lines(sep[,1], h - sep[,2], col = "blue")
    }

    bb = getBBox(lines)
    text(bb$left, h - bb$bottom, bb$text, adj = c(0, 0) )
#    blocks = xmlChildren(p)
#    bb = getBBox(blocks)    
#    rect(bb[, "left"], h - bb[, "bottom"], bb[, "right"], h - bb[, "top"], border = "lightgreen")
}

getBBox =
function(nodes, attrs = c(left = "l", top = "t", right = "r", bottom = "b"), addSuspicious = TRUE)
{
    ans = as.data.frame(t(sapply(nodes, function(x) as.integer(xmlAttrs(x)[attrs]))), row.names = seq(along = nodes))
    names(ans) = names(attrs)
    nm = sapply(nodes, xmlName)
    if(any(nm == "line")) {
        ans$text = sapply(nodes, xmlValue)
        if(addSuspicious)
           ans$numSuspicious = sapply(nodes, function(x) length(getNodeSet(x, ".//x:charParams/@suspicious", "x")))
    }
    if(any(nm == "block"))
        ans$blockType = sapply(nodes, xmlGetAttr, "blockType")
    
    ans
}

getPageDims =
function(p)
{
   structure(as.integer(xmlAttrs(p)[c("width", "height")]), names = c("width", "height"))
}





getLines = getText =
function(doc)
{
    xpathSApply(doc, "//x:line",  xmlValue, namespaces = "x")               
}


getTable =
function(block)
{
    rows = block[ names(block) == "row"]
    do.call(rbind, lapply(rows, processRow))
}

processRow =
function(x)
{
   lapply(x[names(x) == "cell"], processCell)
}

processCell =
function(x)
{
    xmlValue(x)
}

       

getSeparators =
function(nodes, addSep = TRUE)
{
  ans = lapply(nodes, function(x) matrix( c(xmlSApply(x, function(x) c(xmlAttrs(x)[c("x", "y")])), if(addSep) c(NA, NA)), , 2, byrow = TRUE))

  type =  sapply(ans, xmlGetAttr, "type")
  thick =  as.integer(sapply(ans, xmlGetAttr, "thickness")  )
  
  if(addSep) {
      ans = do.call(rbind, ans)
      mode(ans) = "integer"
  }
  
  ans
}


isTextInPicture =
function(text, pics, bb.text = getBBox(text), bb.pics = getBBox(pics))
{
     # Could use k-d trees.
    apply(bb.text[, 1:4], 1, function(x) any(isIn(x, bb.pics)))
}

isIn =
function(pos, boxes)    
{
   w = pos["left"] > boxes[, "left"] &    pos["right"] < boxes[, "right"] &    pos["top"] < boxes[, "top"] &    pos["bottom"] > boxes[, "bottom"]
   
}
