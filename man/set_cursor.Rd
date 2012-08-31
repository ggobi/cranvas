\name{set_cursor}
\alias{set_cursor}
\title{Set the cursor of a view}
\usage{
  set_cursor(view, cursor = "ArrowCursor")
}
\arguments{
  \item{view}{the view for which to change the cursor
  (created by \code{\link[qtpaint]{qplotView}})}

  \item{cursor}{an integer or a character string (see
  Details)}
}
\value{
  \code{NULL}; the cursor of the view is set as a side
  effect
}
\description{
  Change the shape of cursor on a view.
}
\details{
  All possible cursor types with the corresponding integer
  code are:

  0: ArrowCursor; 1: UpArrowCursor; 2: CrossCursor; 3:
  WaitCursor; 4: IBeamCursor; 5: SizeVerCursor; 6:
  SizeHorCursor; 7: SizeBDiagCursor; 8: SizeFDiagCursor; 9:
  SizeAllCursor; 10: BlankCursor; 11: SplitVCursor; 12:
  SplitHCursor; 13: PointingHandCursor; 14:
  ForbiddenCursor; 17: OpenHandCursor; 18:
  ClosedHandCursor; 15: WhatsThisCursor; 16: BusyCursor;
  20: DragMoveCursor; 19: DragCopyCursor; 21:
  DragLinkCursor; 24: BitmapCursor

  We can pass either the integer code or the character
  string to the \code{cursor} argument.
}
\examples{
library(cranvas)
library(qtpaint)
scene = qscene()
qlayer(scene)
v = qplotView(scene = scene)
print(v)

set_cursor(v, 'WaitCursor')

set_cursor(v, 2L)  # CrossCursor
}
\author{
  Yihui Xie <\url{http://yihui.name}>
}
\references{
  \url{http://doc.qt.nokia.com/latest/qt.html#CursorShape-enum}
}

