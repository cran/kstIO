\encoding{UTF-8}
\name{kstIO-package}
\alias{kstIO-package}
\alias{kstIO}
\alias{kstIO file formats}
\docType{package}
\title{\code{kstIO} File formats}

\description{
   Knowledge space theory by Doignon and Falmagne (1985, 1999) 
   is a set- and order-theoretical
   framework, which proposes mathematical formalisms to operationalize 
   knowledge structures in a particular domain.  The 'kstIO' package 
   provides basic functionalities to read and write KST data from/to files.
}

\details{
  This page focuses on the different file formats that can be used with 
  the \code{kstIO} functions.
}

\section{File Formats}{Over time and in different research groups with 
  knowledge space theory, different file formats have evolved.

  \subsection{Matrix Format}{The probably simplest and most direct approach
    is to store the information in a binary ASCII matrix where a "1" 
    in row i and column j means that item j is element of state/response 
    pattern i. 
    
    There is no separating character between the columns, 
    and there should be no trailing whitespace at the end of the line.
    The last line of the matrix must carry an EndOfLine - in most editors
    (except vi) this means an empty line after the matrix.
  }
  \subsection{KST Tools Format}{This format (Hockemeyer, 2001) extends the 
    matrix format by two preceding header lines containing the number of 
    items and the number of states/response patterns, respectively.
  }
  \subsection{SRBT Tools Format}{This format (Poetzi & Wesiak, 2001) extends 
    the KST tools format by yet another preceding header line with format and 
    content metadata. This new header line has the format
    \preformatted{#SRBT v2.0 <struct> ASCII <comment>}
    where \verb{<struct>} specifies the type of data stored in the
    file and \verb{<comment>} is an optional arbitrary comment.

    Furthermore, SRBT files may contain an arbitrary number of comment lines 
    after the number of states/patterns.

    The following data types are supported by the respective
    \code{kstIO} functions:
    \itemize{
      \item basis
      \item data
      \item relation
      \item space
      \item structure
    }
    For \code{kbase} and surmise \code{relation} files, the encoding
    information \code{"ASCII"} is missing because these files are always
    in ASCII format.
  }
  \subsection{CSV Format}{
    CSV (comma separated values) is a standard file format for data tables. Within a row, 
    the different columns are separated by commas. Please note that in some European 
    countries and for some programs, rows are instead separated by semicolon, and the comma
    replaces the decimal point.
    
    CSV files written/read by \code{kstIO} functions start with a head row containing
    the item IDs. The subsequent rows build the matrix as described n the previous
    format descriptions.
  }
}
\section{Special File/Data Types}{
  \subsection{Base Files}{Base files are not available  in KST tools format. 
  
    Their matrix part differs from the other files 
    in that it contains "0", "1", and "2". A "1" means that the state
    is minimal for the item and a "2" means that it is not (but contains 
    the item). A "0" stands (as always) for the state not containing 
    the item.
  }
  \subsection{Surmise Relation Files}{Surmise relation files are not
    available  in KST Tools format, either.

	  Their matrices are somewhat transposed in
	  comparison to all the other formats. Row i and column j is
	  equal to "1" if knowing i can be surmised from knowing j, and 
	  equal to "0" otherwise. Thus, column j describes the minimal
	  state for item j.
  }
  \subsection{Surmise Function Files}{
    Surmise function files are available only in CSV format. 
    
    The matrix is here preceded by a row which denotes the item for 
    which the respective state is a clause.
  }
}
\section{Example}{\preformatted{#SRBT v2.0 structure ASCII
3
5
# Some comment or item information
000
100
110
101
111}
}


\author{Cord Hockemeyer \email{cord.hockemeyer@uni-graz.at}}

\references{
  Doignon, J.-P. & Falmagne, J.-C. (1985). Spaces for the assessment of knowledge. 
  \emph{International Journal of Man-Machine Studies, 23,} 175–196.

  Doignon, J.-P. & Falmagne, J.-C. (1999). \emph{Knowledge Spaces.} 
  Springer Verlag, Berlin.

  Hockemeyer, C. (2001). KST Tools User Manual (2nd ed.).
  \url{https://kst.hockemeyer.at/techreports/KST-Tools_TechRep_FWF01.pdf}.

  Poetzi, S. & Wesiak, G. (2001). SRbT Tools User Manual. 
  \url{https://kst.hockemeyer.at/techreports/SRBT-Tools_TechRep_FWF01.pdf}.
}

\seealso{
  \code{\link[kst]{kbase}}
  \code{\link[kst]{space_property}}
  \code{\link[kst]{kstructure}}
}

