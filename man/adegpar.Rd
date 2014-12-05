\name{adegpar}
\alias{adegpar}

\title{Handling ADEg graphical parameters}

\description{
  \code{adegpar} can be used to set or query graphical parameters used in \code{ADEg} object display.
  
  It is inspired by the \code{par} function of \code{graphics} package. 
}

\usage{
adegpar(...)
}

\arguments{
  \item{\dots}{
    If it is empty, the return value is a named list containing all the current settings.
    
    If it is a string of characters, the corresponding sub-list of parameters is return as information.
    
    If it is a list containing keys and values, the corresponding changes in current settings are made.
  }
}

\details{
  The graphical parameters control apparency of the graphic.
  Calls can be made using either a list of list (e.g. \code{plabels = list(col = "red")}) or a list grouping both keys with "." (e.g. \code{plabels.col = "red"}).
  
  Parameters are re-used if needed in all \code{ADEg} object.
  If set globally, meaning using \code{adegpar}, all created objects afterwards will be affected.
}

\value{
  Several parameters are used to create complete plot and accessible through \code{adegpar}. 
  \describe{
    \item{\code{p1d}:}{parameters for one-dimension graphic, object of class inherited from \code{"ADEg.S1"} or \code{"ADEg.C1"} \itemize{
      \item{\code{horizontal}: a logical indicating if the plot is horizontal}
      \item{\code{reverse}: a logical indicating if the bottom of the plot is at the bottom (for \code{horizontal} as \code{TRUE}) 
        or at the left of the device (for \code{horizontal} as \code{FALSE}). If FALSE, the graphical display bottom is at the top 
        (for \code{horizontal} as \code{TRUE}) or at the right of the device (for \code{horizontal} as \code{FALSE}).}
      \item{\code{rug}: a list dedicated to tick marks \itemize{
        \item{\code{draw}: a logical indicating if the rugs are drawn}
        \item{\code{tck}: size of the rug (ticks) in proportion from the reference line and the origin of the device}
        \item{\code{margin}: where to draw the reference line}
        \item{\code{line}: a logical indicating if the reference line is drawn using \code{porigin} arguments}
      }}
    }}
    
    \item{\code{parrows}:}{arrows' parameters. see \code{panel.arrows} for more information \itemize{
      \item{\code{angle}: angle from the shaft of the arrow to the edge of the arrow head}
      \item{\code{ends}: kind of arrows to be drawn. Can be \code{first}, \code{last} or \code{both}}
      \item{\code{length}: length of the edges of the arrow head}
    }}
    
    \item{\code{paxes}:}{axis' parameters. Mostly inspired by \code{xyplot} function of \code{lattice} package \itemize{
      \item{\code{aspectratio}: a character string to control physical aspect ratio of the graphic (drawing panel more specifically).
        \code{iso} for isometric scales, \code{fill} for drawing as big as possible or \code{xy} for banking rule}
      \item{\code{draw}: a logical indicating if axis (tick marks and labels) are drawn around the graphic}
      \item{\code{x}: a list used for the creation of x-axis in the \code{trellis} object. See \code{xyplot} for more information \itemize{
        \item{\code{draw}: a logical indicating if x-axis (tick marks and labels) are drawn around the graphic}
      }}
      \item{\code{y}: the same list as for \code{x} with \code{draw} parameters}
    }}
    
    \item{\code{pbackground}:}{background's parameters \itemize{
      \item{\code{col}: background color}
      \item{\code{box}: a logical indicating if a box is drawn surrounding the plot}
    }}
    
    \item{\code{pellipses}:}{ellipses' drawing parameters \itemize{
      \item{\code{alpha}: a value between 0 and 1 controlling ellipses' background transparency}
      \item{\code{axes}: a list dedicated to ellipses' axis \itemize{
        \item{\code{draw}: a logical indicating whether ellipses' axis are drawn}
        \item{\code{col}: ellipses' axis color}
        \item{\code{lty}: line type of ellipses' axis}
        \item{\code{lwd}: line width of ellipses' axis}
      }}
      \item{\code{border}: ellipses's border color}
      \item{\code{lty}: line type of ellipses' border}
      \item{\code{lwd}: line width of ellipses' border}
      \item{\code{col}: ellipses' background color}
    }}
    
    \item{\code{pgrid}:}{grid's drawing parameters \itemize{
    	\item{\code{draw}: a logical indicating if grid is drawn in the background}
    	\item{\code{col}: grid's line color}
    	\item{\code{lty}: line type of grid line}
    	\item{\code{lwd}: line width of grid line}
    	\item{\code{nint}: an integer indicating the number of grid intervals expected}
    	\item{\code{text}: a list dedicated to grid legend text\itemize{
        \item{\code{cex}: text size of grid legend}
        \item{\code{col}: text color of grid legend}
        \item{\code{pos}: a character string (\code{topright}, \code{topleft}, \code{bottomleft}, \code{bottomright}) or a vector of length 2 
          indicating text position of grid legend. If it is a vector, the default unit is \code{npc} (normalized parent coordinates).}
      }}
    }}
    
    \item{\code{plabels}:}{labels' drawing parameters \itemize{
      \item{\code{alpha}: a value between 0 and 1 controlling label transparency}
      \item{\code{cex}: labels' text size}
      \item{\code{col}: labels' text color}
      \item{\code{srt}: labels' text orientation. It can be \code{horizontal}, \code{vertical} or an angle indication in degrees}
      \item{\code{optim}: a logical indicating if an algorithm is used to avoid labels' overlapping or outside limits}
      \item{\code{boxes}: label's boxes parameters \itemize{
        \item{\code{draw}: a logical indicating if labels are framed}
        \item{\code{alpha}: a value between 0 and 1 controlling labels' boxes transparency}
        \item{\code{border}: boxes' border color}
        \item{\code{col}: boxes' background color}
        \item{\code{lty}: line type of boxes' border}
        \item{\code{lwd}: line width of boxes' border}
      }}
    }}
    
    \item{\code{plegend}:}{legend's drawing parameters (used for object of class inherited from \code{T.value} and \code{S2.value}) \itemize{
        \item{\code{drawKey}: a logical indicating if the legend should
        be drawn. Legend can be provided by the \code{key} argument or
        is automatically generated for \code{*.class} and \code{*.value}
        functions}
    	\item{\code{drawColorKey}: a logical indicating if the color
        legend should be drawn (only for \code{*.image} functions)}
    	\item{\code{size}: size of the legend}

    }}
    
    \item{\code{plines}:}{lines' drawing parameters \itemize{
      \item{\code{col}: lines color}
      \item{\code{lty}: lines type}
      \item{\code{lwd}: lines width}
    }}
    
    \item{\code{pnb}:}{drawing parameters for neighbourhood graph \itemize{
      \item{\code{edge}: edge's drawing parameters \itemize{
        \item{\code{col}: edge color}
        \item{\code{lty}: line type of edge}
        \item{\code{lwd}: line width of edge}
      }}
      \item{\code{node}: node's drawing parameters \itemize{
        \item{\code{pch}: node's symbol type}
        \item{\code{cex}: node's symbol size}
        \item{\code{col}: node's symbol color}
        \item{\code{alpha}: a value between 0 and 1 controlling node's symbol transparency}
      }}
    }}
    
    \item{\code{porigin}:}{drawing parameters for origin's lines. See \code{panel.lines} for more information \itemize{
    	\item{\code{draw}: a logical indicating if vertical and horizontal lines are drawn to indicate origin}
    	\item{\code{include}: a logical indicating if origin is included in the drawing limits}
    	\item{\code{origin}: a two-length vector indicating origin coordinates}
    	\item{\code{alpha}: a value between 0 and 1 controlling origin's lines transparency}
    	\item{\code{col}: color of origin's lines}
    	\item{\code{lty}: origin's line type}
    	\item{\code{lwd}: origin's line width}
    }}
    
    \item{\code{ppalette}:}{a function taking one integer in argument indicating the number of expecting colors (for example using \code{colorRampPalette}) \itemize{
      \item{\code{quanti}: \code{adegpar()$ppalette$quanti(n)} returns n colors shaded grey to white}
      \item{\code{quali}: \code{adegpar()$ppalette$quali(n, name)} returns \code{n} differentiated colors. \code{name} argument is passed to the \code{brewer.pal} function 
        of the \code{RColorBrewer} package and must be \code{Accent}, \code{Dark2}, \code{Paired}, \code{Pastel1}, \code{Pastel2}, \code{Set1} (the default value), \code{Set2} or \code{Set3}.
        When \code{n} is equal to 2, values for 'white' and 'black' colors are returned and can be not quite visible on the display.}
    }}
    
    \item{\code{ppoints}:}{points' drawing paameters \itemize{
      \item{\code{alpha}: a value between 0 and 1 controlling points transparency}
      \item{\code{cex}: points size}
      \item{\code{col}: points color}
      \item{\code{pch}: points type}
      \item{\code{fill}: points' background color (only for filled points type)}
    }}
    
    \item{\code{ppolygons}:}{polygons' drawing parameters (used for example to draw convex hull for
      \code{S2.class} or Gaussian curves for \code{C1.gauss} objects). See \code{lpolygon} for more information. \itemize{
      \item{\code{border}: polygon's border color}
      \item{\code{col}: polygon's background color}
      \item{\code{lty}: line type of polygon border}
      \item{\code{lwd}: line width of polygon border}
      \item{\code{alpha}: a value between 0 and 1 controlling polygons' background transparency}
    }}
    
    \item{\code{pSp}:}{drawing parameters for spatial object \itemize{
      \item{\code{col}: spatial object's background color}
      \item{\code{border}: spatial object's border color}
      \item{\code{lty}: line type of spatial object border}
      \item{\code{lwd}: line width of spatial object border}
      \item{\code{alpha}: a value between 0 and 1 controlling spatial object transparency}
    }}
    
    \item{\code{psub}:}{subtitle's drawing parameters \itemize{
    	\item{\code{cex}: text size of subtitle}
    	\item{\code{col}: text color of subtitle}
    	\item{\code{position}: a character string (\code{topright}, \code{topleft}, \code{bottomleft}, \code{bottomright}) or a vector of length 2 
          indicating text position of subtitle. If it is a vector, the default unit is \code{npc} (normalized parent coordinates).}
    	\item{\code{text}: the character string to display}
    }}
    
    \item{\code{ptable}:}{for table graphic, object of class inherited from \code{ADEg.T} \itemize{
      \item{\code{x}: x-axis parameters \itemize{ 
        \item{\code{srt}: text rotation}
        \item{\code{pos}: position of the axis. It can be \code{top} or \code{bottom}. Otherwise axis and labels' axis are not drawn}
        \item{\code{tck}: ticks size}
        \item{\code{adj}: justification of labels}
      }}
      \item{\code{y}: same as \code{x} list, but for y-axis \itemize{
        \item{\code{str}, \code{tck}, \code{adj}}
        \item{\code{pos}: position of the axis. It can be \code{left} or \code{right}. Otherwise axis and labels' axis are not drawn}
      }}
      \item{\code{margin}: margin surrounding the drawing panel. The numbers indicate the \code{bottom}, \code{left}, \code{top} and \code{right} margins.
        Results are obtained passing \code{margin} to padding argument in \code{lattice}. Please see 
        \code{layout.heights} and \code{layout.widths} parameters in \code{lattice} package for more information}
    }}
}}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\link{par}}
}

\examples{
oldparamadeg <- adegpar()
    
X <- data.frame(x = runif(50, -1, 2), y = runif(50, -1, 2))
s.label(X)
names(adegpar())
adegpar("paxes.draw", "psub.cex")
adegpar()$pback$col
adegpar("paxes.draw" = TRUE, "psu.ce" = 3, "pback.col" = "grey85")
s.label(X)

adegpar(oldparamadeg)
}

\keyword{list}
\keyword{color}
