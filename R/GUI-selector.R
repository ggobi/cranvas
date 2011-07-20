##' Select a subset of data through a GUI to be brushed.
##' We use a GUI created by \pkg{qtbase} to subset the data based on a
##' given categorical variable. Specifically, we choose certain values
##' of the variable (using the mouse or keyboard) and all the
##' observations which have the same values on this variable will be
##' brushed. This selector can link to any plots based on a mutaframe
##' created by \code{\link{qdata}}.
##' @param data a mutaframe created by \code{\link{qdata}} (with a
##' column \code{.brushed})
##' @param vars character or integer: the variable to be displayed in
##' the data selector (if not specified, the first non-numeric
##' variable will be used; if all columns are numeric, the first
##' column will be used)
##' @return \code{NULL} (a GUI will pop up)
##' @author Yihui Xie and Jason Crowley
##' @seealso \code{\link{qdata}}
##' @export
##' @examples library(cranvas)
##'
##' ## old iris as the toy example
##' qiris = qdata(iris)
##' qparallel(data = qiris)
##' record_selector(data = qiris)
##'
##' ## NRC rankings
##' data(nrcstat)
##' qnrc = qdata(nrcstat)
##' qparallel(vars = 10:13, data = qnrc, main = 'Overview of Rankings', horizontal=FALSE)
##' record_selector('Institution.Name', qnrc)
##' qparallel(vars = 14:19, data = qnrc, main = 'Research, Student Support, Diversity')
##' qparallel(vars = 20:26, data = qnrc, main = 'Publication, Award, Time to Degree')
##'
record_selector = function(vars, data) {
    if (missing(vars)) {
        vars = names(data)[!(sapply(as.data.frame(data), is.numeric))][1]
        if (is.na(vars))
            vars = names(data)[1]
    }
    ## vars should be of length 1
    x = data[, vars[1]]
    xx = as.data.frame(data[!duplicated(x), vars[1], drop = FALSE])

    # instantiate the window
    w = Qt$QWidget()
    w$setWindowTitle("Record Selector")

    # setup the table and link the data model to it
    lst = Qt$QListView()
    lst$setSelectionMode(Qt$QAbstractItemView$ExtendedSelection)
    lst$setAlternatingRowColors(TRUE)
    model = qdataFrameModel(xx)
    lst$setModel(model)
    selModel = lst$selectionModel()

    # set up a handler to update the .brushed column when the index is
    # changed in the table
    qconnect(selModel, "selectionChanged", function(filler1, filler2) {
        currLvls = sapply(selModel$selectedIndexes(),
                           function(i) i$data())
        selected(data) = (x %in% currLvls)
    })

    # set up a search bar and attach an auto-completer to it that is
    # populated with the levels of the variable in question
    le = Qt$QLineEdit()
    comp = Qt$QCompleter(as.character(xx[,1]))
    comp$setCaseSensitivity(Qt$Qt$CaseInsensitive)
    le$setCompleter(comp)

    # set up a handler that will update the selection in the table when
    # return is pressed in the line edit field. clears selection if the
    # text doesn't match a level of the variable. updates .brushed
    qconnect(le, "returnPressed", function() {
        idx = if (le$text != "") {
            grep(le$text, as.character(xx[, 1]))
        } else integer(0)

        if(length(idx) > 1) {
            selModel$select(model$index(idx - 1, 0),
                            Qt$QItemSelectionModel$ClearAndSelect)
        } else selModel$clear()
        selected(data) = idx
    })

    # set the layout of the widgets, and attach it to the window
    lyt = Qt$QVBoxLayout()
    lyt$addWidget(lst)
    lyt$addWidget(le)
    w$setLayout(lyt)

    # make the window visible
    w$show()
}
