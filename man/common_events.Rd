\name{common_key_press}
\alias{common_key_press}
\alias{common_key_release}
\alias{common_mouse_move}
\alias{common_mouse_press}
\alias{common_mouse_release}
\title{Some common processings in the keyboard and mouse events}
\usage{
common_key_press(layer, event, data, meta)

common_key_release(layer, event, data, meta)

common_mouse_press(layer, event, data, meta)

common_mouse_move(layer, event, data, meta)

common_mouse_release(layer, event, data, meta)
}
\arguments{
  \item{layer}{the layer argument in the event callback}

  \item{event}{the event argument in the event callback}

  \item{data}{the data created by \code{\link{qdata}}}

  \item{meta}{the meta data for a plot}
}
\value{
  \code{NULL}
}
\description{
  The key press and release events often involve with
  setting the selection mode of the \code{\link{brush}},
  the alpha transparency, and deleting selected elements,
  and so on. Mouse press, release, move and hover are often
  related to brushing and identifying cases. These
  functions implement these common processes.
}
\section{Mouse Events}{
  Left click to brush the plot with a rectangle brush, and
  right click to resize the brush (the cursor shape will
  become a cross). When the mouse is released, the brush
  history will be saved (\code{\link{save_brush_history}}).
}

\section{Key Events}{
  The keys A, O, X, N and C corresponds to the selection
  mode AND, OR, XOR, NOT and COMPLEMENT respectively.

  Plus (+) and Minus (-) can increase or decrease the alpha
  transparency exponentially.

  The key Delete will make the selected elements invisible,
  and F5 makes all the elements visible.

  The question key (?) toggles the identify mode (on or
  off). The cross cursor shape (+) indicates it is in the
  identify mode, and a normal cursor indicates the brush
  mode.

  In a key release event, we set the selection mode to
  \code{'none'}. If PageUp or PageDown was pressed, we show
  the brush history step by step.
}
\examples{
## see the source code of qbar() or qparallel()
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\seealso{
  \code{\link{brush}}
}
