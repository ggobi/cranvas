##' Select a subset of data through a GUI to be brushed
##'
##' We use a GUI created by \pkg{qtbase} to subset the data based on a
##' given categorical variable. Specifically, we choose certain values
##' of the variable (using the mouse or keyboard) and all the
##' observations which have the same values on this variable will be
##' brushed. This selector can link to any plots based on a mutaframe
##' created by \code{\link{qdata}}.
##'
##' The GUI supports multiple selections when we hold the Shift or
##' Ctrl key. If the character string in the text input box matches
##' with multiple items in the list, all of them will be selected.
##'
##' When we select items in the list, usually a plot based on the same
##' data will get brushed accordingly. On the other hand, when we
##' click on a plot, the corresponding items in the list will be
##' selected as well.
##' @inheritParams qbar
##' @param vars a character string or an integer as a column index, or
##' a variable name (without quotes): the variable to be displayed in
##' the data selector (if not specified, the first non-numeric
##' variable will be used; if all columns are numeric, the first
##' column will be used)
##' @return \code{NULL} (a GUI will pop up)
##' @author Yihui Xie and Jason Crowley
##' @seealso \code{\link{qdata}}
##' @export
##' @example inst/examples/record_selector-ex.R
record_selector = function(vars, data) {
    l = as.list(match.call()[-1])
    if (is.null(l$vars)) {
        vars = names(data)[!(sapply(as.data.frame(data), is.numeric))][1]
        if (is.na(vars))
            vars = names(data)[1]
    } else if (is.symbol(l$vars)) vars = as.character(l$vars)
    if (is.numeric(vars)) vars = names(data)[vars]
    ## vars should be of length 1
    x = data[, vars[1]]
    xx = as.data.frame(data[!duplicated(x), vars[1], drop = FALSE])

    # instantiate the window
    w = Qt$QWidget()
    w$setWindowTitle(paste("Record Selector:", vars))

    # setup the table and link the data model to it
    lst = Qt$QListView()
    lst$setSelectionMode(Qt$QAbstractItemView$ExtendedSelection)
    lst$setAlternatingRowColors(TRUE)
    model = qdataFrameModel(xx)
    lst$setModel(model)
    selModel = lst$selectionModel()

    change1 = change2 = FALSE
    # set up a handler to update the .brushed column when the index is
    # changed in the table
    qconnect(selModel, "selectionChanged", function(filler1, filler2) {
        if (change1) return()
        change2 <<- TRUE
        currLvls = sapply(selModel$selectedIndexes(),
                           function(i) i$data())
        selected(data) = (x %in% currLvls)
        change2 <<- FALSE
    })

    # set up a search bar and attach an auto-completer to it that is
    # populated with the levels of the variable in question
    le = Qt$QLineEdit()
    comp = Qt$QCompleter(as.character(xx[,1]))
    comp$setCaseSensitivity(Qt$Qt$CaseInsensitive)
    le$setCompleter(comp)

    select_items = function(idx) {
            sel <- Qt$QItemSelection(model$index(idx[1],0), model$index(idx[1],0))
            if ((n <- length(idx)) > 1) {
                for (i in 2:n) {
                    sel$select(model$index(idx[i],0),model$index(idx[i],0))
                    sel$select(model$index(idx[i],0),model$index(idx[i],0))
                }
            }
            selModel$select(sel,Qt$QItemSelectionModel$ClearAndSelect)
    }
    # set up a handler that will update the selection in the table when
    # return is pressed in the line edit field. clears selection if the
    # text doesn't match a level of the variable. updates .brushed
    qconnect(le, "returnPressed", function() {
        if (change1) return()
        change2 <<- TRUE
        idx = if (le$text != "") {
            grep(le$text, as.character(xx[, 1]))
        } else integer(0)

        if(length(idx) > 1) select_items(idx-1) else selModel$clear()
        change2 <<- FALSE
    })
    ## let the GUI respond to changes in .brushed too
    d.idx = add_listener(data, function(i, j) {
        if (change2) return()
        change1 <<- TRUE
        if (j == '.brushed') {
            idx = which(xx[, 1] %in% x[selected(data)])
            if (length(idx)) {
                select_items(idx-1)
                lst$scrollTo(model$index(idx[1], 0), Qt$QAbstractItemView$EnsureVisible)
            } else selModel$clear()
        }
        change1 <<- FALSE
    })
    qconnect(w, 'destroyed', function(x) {
        remove_listener(data, d.idx)
    })
    # set the layout of the widgets, and attach it to the window
    lyt = Qt$QVBoxLayout()
    lyt$addWidget(lst)
    lyt$addWidget(le)
    w$setLayout(lyt)

    # make the window visible
    w$show()
}
