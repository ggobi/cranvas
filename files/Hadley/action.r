function(view, description, shortcut, handler) {
  action <- qaction(desc = description, shortcut = shortcut, parent = view)
  qaddAction(view, action)
  qconnect(action, signal = "triggered", handler = handler)  
}
zoominAct <- qaction(desc = "Zoom In",
                     shortcut = "Ctrl++",
                     parent = view)
zoomoutAct <- qaction(desc = "Zoom Out",
                      shortcut = "Ctrl+-",
                      parent = view)

qaddAction(view, zoomoutAct)

qconnect(zoominAct,
         signal = "triggered",
         handler = function(x, ...) {
             qsetTransform(x, scale = 1.2)
         },
         user.data = view)