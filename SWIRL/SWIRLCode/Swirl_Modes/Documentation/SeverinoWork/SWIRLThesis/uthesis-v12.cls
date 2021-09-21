
%***************************************************************
%-->
%--> UTHESIS is a LaTeX document class that facilitates uniform 
%--> dissertations and theses at the University of Toledo. I 
%--> created this package so that UT graduate students do not 
%--> spend time thinking about mundane issues such as formatting 
%--> requirements. Instead, a student can concentrate all their 
%--> effort on the content of their dissertation/thesis --- and 
%--> UTHESIS will quickly transform their draft document into 
%--> one that satisfies all style and formatting requirements 
%--> established by the Graduate Faculty. 
%--> 
%--> Portions of this file were drawn from material included in 
%--> standard LaTeX distributions and from LaTeX code posted    
%--> anonymously on the web.  Authors of such code I can        
%--> identify include: Stephen Page for "doublespace.sty", David
%--> M. Jones for "hangcaption.sty", Leslie Lamport and David   
%--> Carlisle for "ifthen.sty", and David Carlisle for          
%--> "indentfirst.sty".  I am sincerely grateful to these known  
%--> and unknown authors because the material they made        
%--> available will help current and future UT graduate students
%--> prepare their dissertations and theses.                    
%-->                             
%-->                                       --- Mike Dowd
%-->
%***************************************************************

\typeout{=========================================================}
\typeout{============ University of Toledo Thesis Style ==========}
\typeout{======================== UThesis ========================}                                   
\typeout{=========================================================}
\typeout{======================= Mike Dowd =======================}
\typeout{==================== August 22, 2011 ====================}
\typeout{=========================================================}

%#########################################################################
%#########################################################################
%#########################################################################
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{uthesis-v12}
              [1994/12/09 v1.2x
 Standard LaTeX document class]
\newcommand{\@ptsize}{}
\newif\if@restonecol
\newif\if@titlepage
\@titlepagetrue
\newif\if@openright
\newif\if@openbib
\@openbibfalse
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\DeclareOption{a5paper}
   {\setlength\paperheight {210mm}%
    \setlength\paperwidth  {148mm}}
\DeclareOption{b5paper}
   {\setlength\paperheight {250mm}%
    \setlength\paperwidth  {176mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{legalpaper}
   {\setlength\paperheight {14in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{executivepaper}
   {\setlength\paperheight {10.5in}%
    \setlength\paperwidth  {7.25in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
\DeclareOption{10pt}{\renewcommand{\@ptsize}{2}}
\DeclareOption{11pt}{\renewcommand{\@ptsize}{2}}
\DeclareOption{12pt}{\renewcommand{\@ptsize}{2}}
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\DeclareOption{final}{\setlength\overfullrule{0pt}}
\DeclareOption{titlepage}{\@titlepagetrue}
\DeclareOption{notitlepage}{\@titlepagefalse}
\DeclareOption{openright}{\@openrighttrue}
\DeclareOption{openany}{\@openrightfalse}
\DeclareOption{onecolumn}{\@twocolumnfalse}
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}
\DeclareOption{openbib}{\@openbibtrue}
\ExecuteOptions{letterpaper,10pt,oneside,onecolumn,final,openany}
\ProcessOptions
\input{size1\@ptsize.clo}
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand{\baselinestretch}{}
\setlength\parskip{0\p@ \@plus \p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand{\topfraction}{.7}
\setcounter{bottomnumber}{1}
\renewcommand{\bottomfraction}{.3}
\setcounter{totalnumber}{3}
\renewcommand{\textfraction}{.2}
\renewcommand{\floatpagefraction}{.5}
\setcounter{dbltopnumber}{2}
\renewcommand{\dbltopfraction}{.7}
\renewcommand{\dblfloatpagefraction}{.5}
\if@twoside
  \def\ps@headings{%
      \let\@oddfoot\@empty\let\@evenfoot\@empty
      \def\@evenhead{\thepage\hfil\slshape\leftmark}%
      \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
      \let\@mkboth\markboth
    \def\chaptermark##1{%
      \markboth {\uppercase{%
        \ifnum \c@secnumdepth >\m@ne
            \@chapapp\ \thechapter. \ %
        \fi
        ##1}}{}}%
    \def\sectionmark##1{%
      \markright {\uppercase{%
        \ifnum \c@secnumdepth >\z@
          \thesection. \ %
        \fi
        ##1}}}}
\else
  \def\ps@headings{%
    \let\@oddfoot\@empty
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\markboth
    \def\chaptermark##1{%
      \markright {\uppercase{%
        \ifnum \c@secnumdepth >\m@ne
            \@chapapp\ \thechapter. \ %
        \fi
        ##1}}}}
\fi
\def\ps@myheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\@gobbletwo
    \let\chaptermark\@gobble
    \let\sectionmark\@gobble
    }
\if@titlepage
  \newcommand{\maketitle}{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \null\vfil
  \vskip 60\p@
  \begin{center}%
    {\LARGE \@title \par}%
    \vskip 3em%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
      \vskip 1.5em%
    {\large \@date \par}%       % Set date in \large size.
  \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \let\thanks\relax\let\maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}}
\else
\newcommand{\maketitle}{\par
  \begingroup
    \renewcommand{\thefootnote}{\fnsymbol{footnote}}%
    \def\@makefnmark{\hbox to\z@{$\m@th^{\@thefnmark}$\hss}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
            \hbox to1.8em{\hss$\m@th^{\@thefnmark}$}##1}%
    \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
      \fi
    \else
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \thispagestyle{plain}\@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \let\thanks\relax
  \let\maketitle\relax\let\@maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}}
\def\@maketitle{%
  \newpage
  \null
  \vskip 2em%
  \begin{center}%
    {\LARGE \@title \par}%
    \vskip 1.5em%
    {\large
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author 
      \end{tabular}\par}%
    \vskip 1em%
    {\large \@date}%
  \end{center}%
  \par
  \vskip 1.5em}
\fi
\newcommand*{\chaptermark}[1]{}
\setcounter{secnumdepth}{2}
\newcounter {part}
\newcounter {chapter}
\newcounter {section}[chapter]
\newcounter {subsection}[section]
\newcounter {subsubsection}[subsection]
\newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph]
\renewcommand{\thepart}         {\Roman{part}}
\renewcommand{\thechapter}      {\arabic{chapter}}
\renewcommand{\thesection}      {\thechapter.\arabic{section}}  
\renewcommand{\thesubsection}   {\thesection.\arabic{subsection}}
\renewcommand{\thesubsubsection}{\thesubsection .\arabic{subsubsection}}
\renewcommand{\theparagraph}    {\thesubsubsection.\arabic{paragraph}}
\renewcommand{\thesubparagraph} {\theparagraph.\arabic{subparagraph}}
\newcommand{\@chapapp}{\chaptername}
\newcommand{\part}{\cleardoublepage
                 \thispagestyle{plain}%
                 \if@twocolumn
                     \onecolumn
                     \@tempswatrue
                   \else
                     \@tempswafalse
                 \fi
                 \hbox{}\vfil
                 \secdef\@part\@spart}

\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >-2\relax
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    \markboth{}{}
    {\centering
     \interlinepenalty \@M
     \reset@font
     \ifnum \c@secnumdepth >-2\relax
       \huge\bfseries \partname~\thepart
       \par
       \vskip 20\p@
     \fi
     \Huge \bfseries #2\par}%
    \@endpart}
\def\@spart#1{%
    {\centering
     \interlinepenalty \@M
     \reset@font
     \Huge \bfseries #1\par}%
    \@endpart}
\def\@endpart{\vfil\newpage
              \if@twoside
                \hbox{}%
                \thispagestyle{empty}%
                \newpage
              \fi
              \if@tempswa
                \twocolumn
              \fi}
\newcommand{\chapter}{\if@openright\cleardoublepage\else\clearpage\fi
                   %%%%%%%%%%%%%%%%%%% \thispagestyle{plain}%
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter}
\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                         \refstepcounter{chapter}%
                         \typeout{\@chapapp\space\thechapter.}%
                         \addcontentsline{toc}{chapter}%
                                   {\protect\numberline{\thechapter}#1}%
                    \else
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}% 
                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading 
                    \fi}
\def\@makechapterhead#1{%
  \vspace*{22\p@}% %%%%%%%% CHANGED FROM \vspace*{50\p@}%
  {\parindent \z@ \raggedright \reset@font
    \ifnum \c@secnumdepth >\m@ne
        \huge\bfseries \@chapapp{} \thechapter
        \par\nobreak
        \vskip 22\p@% %%%%%%%% CHANGED FROM \vskip 20\p@%
                      %%%%%%%% This \vskip command controls the vertical 
                      %%%%%%%% space between the chapter number and the 
                      %%%%%%%% chapter heading.
    \fi
    \interlinepenalty\@M
    \Huge \bfseries #1\par\nobreak
    \vskip 48\p@% %%%%%%%% CHANGED FROM \vskip 40\p@
                  %%%%%%%% In contrast to that below, this \vskip command
                  %%%%%%%% controls the vertical space between the chapter
                  %%%%%%%% heading and the subsequent text --- When There
                  %%%%%%%% Is A Chapter Number (e.g., "Chapter 1").
  }}
\def\@schapter#1{\if@twocolumn
                   \@topnewpage[\@makeschapterhead{#1}]%
                 \else
                   \@makeschapterhead{#1}%
                   \@afterheading
                 \fi}
\def\@makeschapterhead#1{%
  \vspace*{22\p@}% %%%%%%%% CHANGED FROM \vspace*{50\p@}%
  {\parindent \z@ \raggedright
    \reset@font
    \interlinepenalty\@M
    \Huge \bfseries  #1\par\nobreak
    \vskip 24\p@% %%%%%%%% CHANGED FROM \vskip 40\p@ 
                  %%%%%%%% In contrast to that above, this \vskip command
                  %%%%%%%% controls the vertical space between the chapter
                  %%%%%%%% heading and the subsequent text --- When There
                  %%%%%%%% Is NO Chapter Number.
  }}
\newcommand{\section}{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\reset@font\Large\bfseries}}
\newcommand{\subsection}{\@startsection{subsection}{2}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\reset@font\large\bfseries}}
\newcommand{\subsubsection}{\@startsection{subsubsection}{3}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\reset@font\normalsize\bfseries}}
\newcommand{\paragraph}{\@startsection{paragraph}{4}{\z@}%
                                    {3.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\reset@font\normalsize\bfseries}}
\newcommand{\subparagraph}{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\reset@font\normalsize\bfseries}}
\if@twocolumn
  \setlength\leftmargini  {2em}
\else
  \setlength\leftmargini  {2.5em}
\fi
\setlength\leftmarginii  {2.2em}
\setlength\leftmarginiii {1.87em}
\setlength\leftmarginiv  {1.7em}
\if@twocolumn
  \setlength\leftmarginv  {.5em}
  \setlength\leftmarginvi {.5em}
\else
  \setlength\leftmarginv  {1em}
  \setlength\leftmarginvi {1em}
\fi
\setlength\leftmargin    {\leftmargini}
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\renewcommand{\theenumi}{\arabic{enumi}}
\renewcommand{\theenumii}{\alph{enumii}}
\renewcommand{\theenumiii}{\roman{enumiii}}
\renewcommand{\theenumiv}{\Alph{enumiv}}
\newcommand{\labelenumi}{\theenumi.}
\newcommand{\labelenumii}{(\theenumii)}
\newcommand{\labelenumiii}{\theenumiii.}
\newcommand{\labelenumiv}{\theenumiv.}
\renewcommand{\p@enumii}{\theenumi}
\renewcommand{\p@enumiii}{\theenumi(\theenumii)}
\renewcommand{\p@enumiv}{\p@enumiii\theenumiii}
\newcommand{\labelitemi}{$\m@th\bullet$}
\newcommand{\labelitemii}{\normalfont\bfseries --}
\newcommand{\labelitemiii}{$\m@th\ast$}
\newcommand{\labelitemiv}{$\m@th\cdot$}
\newenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand{\descriptionlabel}[1]{\hspace\labelsep
                                \normalfont\bfseries #1}
\if@titlepage
  \newenvironment{abstract}{%
      \titlepage
      \null\vfil
      \begin{center}
        \bfseries \abstractname
      \end{center}}
     {\par\vfil\null\endtitlepage}
\else
  \newenvironment{abstract}{%
      \if@twocolumn
        \section*{\abstractname}%
      \else
        \small
        \begin{center}%
          {\bfseries \abstractname\vspace{-.5em}\vspace{\z@}}%
        \end{center}%
        \quotation
      \fi}
      {\if@twocolumn\else\endquotation\fi}
\fi
\newenvironment{verse}
               {\let\\=\@centercr
                \list{}{\itemsep      \z@
                        \itemindent   -1.5em%
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \advance\leftmargin 1.5em}%
                \item[]}
               {\endlist}
\newenvironment{quotation}
               {\list{}{\listparindent 1.5em%
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item[]}
               {\endlist}
\newenvironment{quote}
               {\list{}{\rightmargin\leftmargin}%
                \item[]}
               {\endlist}
\newenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
     %%%%%%%%%%%%%%%% \thispagestyle{empty}%
      \if@compatibility
        \setcounter{page}{0}
      \else
        \setcounter{page}{1}%
      \fi}%
    {\if@restonecol\twocolumn \else \newpage \fi
      \setcounter{page}{1}%
    }
\newcommand{\appendix}{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \renewcommand{\@chapapp}{\appendixname}%
  \renewcommand{\thechapter}{\Alph{chapter}}}
\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}
\@addtoreset{equation}{chapter}
\renewcommand{\theequation}{\thechapter.\arabic{equation}}
\newcounter{figure}[chapter]
\renewcommand{\thefigure}{\thechapter.\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename~\thefigure}
\newenvironment{figure}
               {\@float{figure}}
               {\end@float}
\newenvironment{figure*}
               {\@dblfloat{figure}}
               {\end@dblfloat}
\newcounter{table}[chapter]
\renewcommand{\thetable}{\thechapter.\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename~\thetable}
\newenvironment{table}
               {\@float{table}}
               {\end@float}
\newenvironment{table*}
               {\@dblfloat{table}}
               {\end@dblfloat}
\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{10\p@}
\setlength\belowcaptionskip{0\p@}
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{#1: #2}%
  \ifdim \wd\@tempboxa >\hsize
    #1: #2\par
  \else
    \global \@minipagefalse
    \hbox to\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*{\cal}{\@fontswitch{\relax}{\mathcal}}
\DeclareRobustCommand*{\mit}{\@fontswitch{\relax}{\mathnormal}}
\newcommand{\@pnumwidth}{1.55em}
\newcommand{\@tocrmarg} {2.55em}
\newcommand{\@dotsep}{4.5}
\setcounter{tocdepth}{3}
\newcommand{\tableofcontents}{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\contentsname\vspace{0.18in}
        \@mkboth{\uppercase{\contentsname}}{\uppercase{\contentsname}}}%  %%% I ADDED THE \vspace{0.18in}
        \addcontentsline{toc}{chapter}{\contentsname}
    \@starttoc{toc}%
    \if@restonecol\twocolumn\fi
    }
\newcommand*{\l@part}[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \addvspace{2.25em \@plus\p@}%
    \begingroup
      \setlength\@tempdima{3em}%
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \large \bfseries #1\hfil \hbox to\@pnumwidth{\hss #2}}\par
       \nobreak
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}
    \endgroup
  \fi}
\newcommand*{\l@chapter}[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \vskip 1.0em \@plus\p@
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
      \penalty\@highpenalty
    \endgroup
  \fi}
\newcommand*{\l@section}      {\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand*{\l@subsection}   {\@dottedtocline{2}{3.8em}{3.2em}}
\newcommand*{\l@subsubsection}{\@dottedtocline{3}{7.0em}{4.1em}}
\newcommand*{\l@paragraph}    {\@dottedtocline{4}{10em}{5em}}
\newcommand*{\l@subparagraph} {\@dottedtocline{5}{12em}{6em}}
\newcommand{\listoffigures}{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listfigurename\vspace{0.18in}
      \@mkboth{\uppercase{\listfigurename}}%
              {\uppercase{\listfigurename}}}%  %%% I ADDED THE \vspace{0.18in}
    \addcontentsline{toc}{chapter}{\listfigurename}%%%%%%%% I ADDED THIS
    \@starttoc{lof}%
    \if@restonecol\twocolumn\fi
    }
\newcommand*{\l@figure}{\@dottedtocline{1}{0em}{2.3em}}
\newcommand{\listoftables}{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listtablename\vspace{0.18in}
      \@mkboth{\uppercase{\listtablename}}{\uppercase{\listtablename}}}%  %%% I ADDED THE \vspace{0.18in}
    \addcontentsline{toc}{chapter}{\listtablename}
    \@starttoc{lot}%
    \if@restonecol\twocolumn\fi
    }
\let\l@table\l@figure
\newdimen\bibindent
\bibindent=1.5em
\newenvironment{thebibliography}[1]
     {\chapter*{References %%% \bibname %%% THIS GUARANTEES THE SECTION
                                        %%% HEADING WILL BE "REFERENCES"
                                        %%% (THIS IS MANDATED BY THE
                                        %%% GRADUATE FACULTY)
        \addcontentsline{toc}{chapter}{References} %%% I ADDED THIS LINE
        \@mkboth{\uppercase{\bibname}}{\uppercase{\bibname}}}%
      \list{\@biblabel{\arabic{enumiv}}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \if@openbib
              \advance\leftmargin\bibindent
              \itemindent -\bibindent
              \listparindent \itemindent
              \parsep \z@
            \fi
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand{\theenumiv}{\arabic{enumiv}}}%
      \if@openbib
        \renewcommand{\newblock}{\par}
      \else
        \renewcommand{\newblock}{\hskip .11em \@plus.33em \@minus.07em}%
      \fi
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.=\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
\newcommand{\newblock}{}
\newenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\@makeschapterhead{\indexname}]%
                \@mkboth{\uppercase{\indexname}}%
                        {\uppercase{\indexname}}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}
\newcommand{\@idxitem}  {\par\hangindent 40\p@}
\newcommand{\subitem}   {\par\hangindent 40\p@ \hspace*{20\p@}}
\newcommand{\subsubitem}{\par\hangindent 40\p@ \hspace*{30\p@}}
\newcommand{\indexspace}{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}
\renewcommand{\footnoterule}{%
  \kern-3\p@
  \hrule width .4\columnwidth
  \kern 2.6\p@}
\@addtoreset{footnote}{chapter}
\long\def\@makefntext#1{%
    \parindent 1em%
    \noindent
    \hbox to 1.8em{\hss\@makefnmark}#1}
\newcommand{\contentsname}{Contents}
\newcommand{\listfigurename}{List of Figures}
\newcommand{\listtablename}{List of Tables}
\newcommand{\bibname}{References} %%% SEE ABOVE COMMAND
                                  %%% "\newenvironment{thebibliography}[1]"
                                  %%% I RENDER \bibname MOOT (AS IS MANDATED
                                  %%% BY THE GRADUATE FACULTY).
\newcommand{\indexname}{Index}
\newcommand{\figurename}{Figure}
\newcommand{\tablename}{Table}
\newcommand{\partname}{Part}
\newcommand{\chaptername}{Chapter}
\newcommand{\appendixname}{Appendix}
\newcommand{\abstractname}{Abstract}
\newcommand{\today}{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagestyle{plain}
\pagenumbering{arabic}      % Arabic page numbers
\if@twoside
\else
  \raggedbottom
\fi
\if@twocolumn
  \twocolumn
  \sloppy
  \flushbottom
\else
  \onecolumn
\fi

%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################

\typeout{=========================================================}
\typeout{============ University of Toledo Thesis Style ==========}
\typeout{=========================================================}
\typeout{================= }
\typeout{================= Let the magic begin ...}
\typeout{================= }
\typeout{=========================================================}

%#########################################################################
%#########################################################################
%#########################################################################
%#############################
%#############################  Set the margins
%#############################
%#############################  Do NOT alter these values --- the
%#############################  Graduate College is watching
%#############################

\oddsidemargin 0.5in    \evensidemargin 0.0in   \textwidth 6.0in 
\textheight 8.9in       \marginparwidth 0.0in   \marginparsep 0.0in 
\topmargin 0.0in        \headheight 0.0in       \headsep 0.0in 
\footskip 0.32in        \topskip 0.0in        % \footheight 0.15in   

%#########################################################################
%#########################################################################
%#########################################################################

\typeout{------------> borrowing Lamport's and Carlisle's ifthen.sty}

%% File `ifthen.sty'
%% Authors: Leslie Lamport and David Carlisle
%% Copyright (c) 1991 by Leslie Lamport
%% Copyright (c) 1994-2001 LaTeX3 project, David Carlisle

\def\TE@throw{\@ne=\@ne\noexpand\fi}
\def\boolean#1#2{%
  \TE@throw\expandafter\noexpand\csname if#1\endcsname#2}
\def\TE@length#1#2{\TE@throw\noexpand\ifdim#1#2}
\def\TE@odd#1#2{%
  \TE@throw\noexpand\TE@@odd#1\noexpand\@nil\noexpand\ifodd\count@#2}
\def\TE@@odd#1#2\@nil{%
  \@defaultunits
  \count@\if-#1-0\else0\expandafter#1\fi#2\relax\@nnil}
\def\TE@repl#1#2{%
  \long\def\@tempc##1#1##2{%
    \def\@tempa{##2}\def\@tempb{\@tempc}%
    \ifx\@tempa\@tempb
     \toks@\expandafter{\the\toks@##1}%
     \expandafter\@gobble
    \else
      \toks@\expandafter{\the\toks@##1#2}%
    \expandafter\@tempc
    \fi
    ##2}%
  \expandafter\toks@\expandafter{\expandafter}%
  \expandafter\@tempc\the\toks@#1\@tempc}
\long\def\ifthenelse#1{%
  \toks@{#1}%
  \TE@repl\or\TE@or
  \TE@repl\and\TE@and
  \TE@repl\not\TE@neg
  \TE@repl\OR\TE@or
  \TE@repl\AND\TE@and
  \TE@repl\NOT\TE@neg
    \begingroup
        \let\protect\@unexpandable@protect
        \def\@setref##1##2##3{%
         \ifx##1\relax\z@\else\expandafter##2##1\fi}%
        \def\value##1{\the\csname c@##1\endcsname}%
        \let\equal\TE@equal \let\(\TE@lparen \let\)\TE@rparen
        \let\isodd\TE@odd \let\lengthtest\TE@length
        \let\isundefined\TE@undef
        \begingroup
          \let\@tempa\relax\let\@tempb\relax
          \xdef\@gtempa{\expandafter\TE@eval\the\toks@\TE@endeval}%
        \endgroup
        \@gtempa
        \expandafter\endgroup\ifTE@val
          \expandafter\@firstoftwo
        \else
          \expandafter\@secondoftwo
        \fi}
\def\TE@eval{\noexpand\TE@negatefalse\noexpand\iftrue\noexpand\ifnum}
\newif\ifTE@val
\newif\ifTE@negate
\def\TE@endeval{\relax
      \noexpand\TE@setvaltrue\noexpand
    \else
      \noexpand\TE@setvalfalse\noexpand
    \fi
    \noexpand\TE@negatefalse\noexpand
  \fi}
\def\TE@setvaltrue{%
  \ifTE@negate\TE@valfalse\else\TE@valtrue\fi}
\def\TE@setvalfalse{\let\ifTE@val\ifTE@negate}
\def\TE@or{\TE@endeval\noexpand\ifTE@val\noexpand\else\noexpand\ifnum}
\def\TE@and{\TE@endeval\noexpand\ifTE@val\noexpand\ifnum}
\def\TE@neg{\TE@throw\noexpand\TE@negswitch\noexpand\ifnum}
\def\TE@negswitch{\ifTE@negate\TE@negatefalse\else\TE@negatetrue\fi}
\def\TE@lparen#1{\TE@throw\begingroup\TE@eval#1}
\def\TE@rparen#1{%
  \TE@endeval
  \noexpand\expandafter\endgroup\noexpand\ifTE@val#1}
\long\def\TE@equal#1#2#3{\TE@throw
      \def\@tempa{#1}\def\@tempb{#2}%
      \noexpand\ifx\@tempa\@tempb#3}
\def\setboolean#1#2{%
  \lowercase{\def\@tempa{#2}}%
  \@ifundefined{@tempswa\@tempa}%
    {\PackageError{ifthen}%
       {You can only set a boolean to `true' or `false'}\@ehc}%
    {\@ifundefined{#1\@tempa}%
      {\PackageError{ifthen}{Boolean #1 undefined}\@ehc}%
      {\csname#1\@tempa\endcsname}}}
\def\newboolean#1{%
  \expandafter\@ifdefinable\csname if#1\endcsname{%
    \expandafter\newif\csname if#1\endcsname}}
\def\provideboolean#1{%
  \@ifundefined{if#1}{%
    \expandafter\newif\csname if#1\endcsname}\relax}
\long\def\whiledo#1#2{%
  \ifthenelse{#1}%
    {\@whiledotrue
     \@whilesw\if@whiledo\fi
       {#2%
        \ifthenelse{#1}\@whiledotrue\@whiledofalse}}%
    {}%
  }
\def\TE@undef#1#2{%
  \TE@throw\noexpand\ifx\noexpand\@undefined\noexpand#1#2}
\newif\if@whiledo
%% \endinput
%%
%% End of file `ifthen.sty'.

%#########################################################################
%#########################################################################
%#########################################################################

\typeout{------------> borrowing Stephen Page's doublespace.sty}

%
% Author: Stephen Page  -- sdpage@prg.oxford.ac.uk
%                          sdpage%prg.oxford.ac.uk@cs.ucl.ac.uk
%
% Release date: 6 July 1986 - September 3, 1986

\def\baselinestretch{2}

\def\setstretch#1{\renewcommand{\baselinestretch}{#1}}

\def\@setsize#1#2#3#4{\@nomath#1%
   \let\@currsize#1\baselineskip
   #2\baselineskip\baselinestretch\baselineskip
   \parskip\baselinestretch\parskip
   \setbox\strutbox\hbox{\vrule height.7\baselineskip
      depth.3\baselineskip width\z@}%
   \normalbaselineskip\baselineskip#3#4}

\skip\footins 20pt plus4pt minus4pt

\def\@xfloat#1[#2]{\ifhmode \@bsphack\@floatpenalty -\@Mii\else
   \@floatpenalty-\@Miii\fi\def\@captype{#1}\ifinner
      \@parmoderr\@floatpenalty\z@
    \else\@next\@currbox\@freelist{\@tempcnta\csname ftype@#1\endcsname
       \multiply\@tempcnta\@xxxii\advance\@tempcnta\sixt@@n
       \@tfor \@tempa :=#2\do
                        {\if\@tempa h\advance\@tempcnta \@ne\fi
                         \if\@tempa t\advance\@tempcnta \tw@\fi
                         \if\@tempa b\advance\@tempcnta 4\relax\fi
                         \if\@tempa p\advance\@tempcnta 8\relax\fi
         }\global\count\@currbox\@tempcnta}\@fltovf\fi
    \global\setbox\@currbox\vbox\bgroup 
    \def\baselinestretch{1}\small\normalsize
    \boxmaxdepth\z@
    \hsize\columnwidth \@parboxrestore}
\long\def\@footnotetext#1{\insert\footins{\def\baselinestretch{1}\footnotesize
    \interlinepenalty\interfootnotelinepenalty 
    \splittopskip\footnotesep
    \splitmaxdepth \dp\strutbox \floatingpenalty \@MM
    \hsize\columnwidth \@parboxrestore
   \edef\@currentlabel{\csname p@footnote\endcsname\@thefnmark}\@makefntext
    {\rule{\z@}{\footnotesep}\ignorespaces
      #1\strut}}}

\def\singlespace{%
\vskip\parskip%
\vskip\baselineskip%
\def\baselinestretch{1}%
\ifx\@currsize\normalsize\@normalsize\else\@currsize\fi%
\vskip-\parskip%
\vskip-\baselineskip%
}

\def\endsinglespace{\par}

\def\spacing#1{\par%
 \def\baselinestretch{#1}%
 \ifx\@currsize\normalsize\@normalsize\else\@currsize\fi}

\def\endspacing{\par%
 \vskip \parskip%
 \vskip \baselineskip%
 \endgroup%
 \vskip -\parskip%
 \vskip -\baselineskip%
 \begingroup}

\def\onehalfspace{\spacing{1.5}}
\let\endonehalfspace=\endspacing

\def\doublespace{\spacing{2}}
\let\doublespace=\endspacing

\everydisplay{
   \abovedisplayskip \baselinestretch\abovedisplayskip%
   \belowdisplayskip \abovedisplayskip%
   \abovedisplayshortskip \baselinestretch\abovedisplayshortskip%
   \belowdisplayshortskip  \baselinestretch\belowdisplayshortskip}

%***********************************************************
%*********
%*********  The following sets the appropriate value of
%*********  \setstretch{value} for double spacing. This
%*********  value is determined by the font point size
%*********  chosen by the user of UThesis (i.e., 10pt, 11pt,
%*********  or 12pt). As Goossens, Mittelbach, and Samarin
%*********  (1993, p. 470) indicates, this adjustment is
%*********  necessary because the simple "\setstretch{2}"
%*********  command adds approximately 20% additional space
%*********  per line.
%*********
\ifthenelse{\@ptsize = 0}{ \setstretch{1.67} }{}
\ifthenelse{\@ptsize = 1}{ \setstretch{1.62} }{}
\ifthenelse{\@ptsize = 2}{ \setstretch{1.66} }{}  

%#########################################################################
%#########################################################################
%#########################################################################

\typeout{------------> borrowing David M. Jones' hangcaption.sty}

% David M. Jones
% MIT Laboratory for Computer Science
% Room NE43-316
% 545 Technology Square
% Cambridge, MA 02139
% USA

\newlength\captionwidth \captionwidth=\hsize

\def\hangcaption{\refstepcounter\@captype\@dblarg{\@hangcaption\@captype}}

\long\def\@hangcaption#1[#2]#3{%
  \baselineskip 14.5\p@
  \addcontentsline{\csname ext@#1\endcsname}{#1}%
    {\protect\numberline{\csname the#1\endcsname}{\ignorespaces #2}}%
  \par
  \begingroup
    \@parboxrestore
    \setbox\@tempboxa\hbox{\csname fnum@#1\endcsname: #3}%
    \ifdim \wd\@tempboxa > \captionwidth
      \begingroup
        \setbox\@tempboxa\hbox{\csname fnum@#1\endcsname: }%
        \advance \captionwidth by -\wd\@tempboxa
        \@makecaption{\csname fnum@#1\endcsname}%
            {\ignorespaces{\protect\parbox[t]{\captionwidth}{\leavevmode#3}}}%
      \endgroup
    \else
      \begingroup
        \@makecaption{\csname fnum@#1\endcsname}{\ignorespaces #3}%
      \endgroup
    \fi
    \par\vspace{1.7ex}%
  \endgroup}

\let\isucaption\hangcaption

%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                          %%% 
%%% \let\caption\hangcaption %%% This would force ALL captions to be hangcaptions
%%%                          %%% 
%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% \let\captionwidth=\textwidth

%#########################################################################
%#########################################################################
%#########################################################################

\typeout{------------> UT Thesis Specifications}

%#########################################################################
%#########
%#########  ERROR COMMANDS
%#########  
 
\def\NoCommandError#1{\fbox{\large\bf Error: $\backslash$#1\{\} was omitted}}
\def\EmptyCommandError#1{\fbox{\large\bf Error: $\backslash$#1\{\} is empty}}

\def\NoMULTCommandError#1{\fbox{\bf Error: $\backslash$#1\{\}\{\} was omitted}}
\def\EmptyMULTCommandError#1{\fbox{\bf Error: $\backslash$#1\{\}\{\} is empty}}

%#########################################################################
%#########
%#########  UNIVERSITY TITLE PAGE
%#########  

\def\Treatise{\NoCommandError{mydocument}}                       
\def\mydocumentERROR{\fbox{\large\bf ~Incorrect (or misspelled) entry in $\backslash$mydocument\{\}~}}
\newcommand{\mydocument}[1]{\ifthenelse{\equal{#1}{} } % 1st IF
                                       {\def\Treatise{\Emptymydocument} } % ............................... THEN of 1st IF
                                       { \ifthenelse{\equal{#1}{Dissertation} } % ......................... BEGIN Else of 1st IF; 2nd IF
                                                    {\def\Treatise{#1}} % ................................. THEN of 2nd IF
                                                    {\ifthenelse{\equal{#1}{Thesis}} % .................... BEGIN Else of 2nd IF; 3rd IF
                                                        {\def\Treatise{Thesis}} %.......................... THEN of 3rd IF
                                                        {\ifthenelse{\equal{#1}{Project}} % ............... BEGIN Else of 3nd IF; 4th IF
                                                            {\def\Treatise{Project}} %..................... THEN of 4th IF
                                                            {\def\Treatise{\mydocumentERROR}} %............ ENDS ELSE of 4th IF            
                                                        } %................................................ ENDS ELSE of 3rd IF
                                                    }  % .................................................. ENDS Else of 2nd IF
                                       } % ................................................................ ENDS Else of 1st IF
                            } % ........................................................................... ENDS \mydocument definition
%-------------------------
\def\DegreeLevel{\NoMULTCommandError{degree} }
\def\DegreeDiscipline{\NoMULTCommandError{degree} }
\def\EmptydegreeLEVEL{\EmptyMULTCommandError{degree} }
\def\EmptydegreeDISCIPLINE{\EmptyMULTCommandError{degree} }
\newcommand{\degree}[2]{\ifthenelse{\equal{#1}{} }
                                       {\def\DegreeLevel{\mbox{\EmptydegreeLEVEL }}}
                                       {\def\DegreeLevel{\mbox{#1}} }
                               \ifthenelse{\equal{#2}{} }
                                       {\def\DegreeDiscipline{\mbox{\EmptydegreeDISCIPLINE }}}
                                       {\def\DegreeDiscipline{\mbox{#2}} }
                               } % Ends \degree definition                                        
%-------------------------
\def\AwardMonth{\NoMULTCommandError{conferraldate} }
\def\AwardYear{\NoMULTCommandError{conferraldate} }
\def\EmptyconferraldateMONTH{\EmptyMULTCommandError{conferraldate} }
\def\EmptyconferraldateYEAR{\EmptyMULTCommandError{conferraldate} }
\newcommand{\conferraldate}[2]{\ifthenelse{\equal{#1}{} }
                                       {\def\AwardMonth{\mbox{\EmptyconferraldateMONTH }}}
                                       {\def\AwardMonth{\mbox{#1}} }
                               \ifthenelse{\equal{#2}{} }
                                       {\def\AwardYear{\mbox{\EmptyconferraldateYEAR }}}
                                       {\def\AwardYear{\mbox{#2}} }
                               } % Ends \conferraldate definition   
%-------------------------
\def\Advisor{\NoCommandError{advisor} }
\def\Emptyadvisor{\EmptyCommandError{advisor} }
\newcommand{\advisor}[1]{\ifthenelse{\equal{#1}{} }
                                      {\def\Advisor{\mbox{\Emptyadvisor}}}
                                      {\def\Advisor{\mbox{#1}} }}
%-------------------------
\def\Dean{\NoMULTCommandError{graduatedean} }
\def\Emptygraduatedean{\EmptyMULTCommandError{graduatedean} }
\def\DeanTitle{\NoMULTCommandError{graduatedeanTitle} }
\def\EmptygraduatedeanTitle{\EmptyMULTCommandError{graduatedean} }
\newcommand{\graduatedean}[2]{\ifthenelse{\equal{#1}{} }
                                     {\def\Dean{\mbox{\Emptygraduatedean}}}
                                     {\def\Dean{\mbox{#1}} }
                         \ifthenelse{\equal{#2}{} }
                                     {\def\DeanTitle{\mbox{\EmptygraduatedeanTitle}}}
                                     {\def\DeanTitle{\mbox{#2}} }
                         } % Ends graduatedean definition
%-------------------------

\def\addSecondMember{}  \def\addFifthMember{}    \def\addEighthMember{}
\def\addThirdMember{}   \def\addSixthMember{}    
\def\addFourthMember{}  \def\addSeventhMember{} 

\def\sigEXP{\rule{0pt}{2em}}   \def\TitleLineSkip{\\}           
\def\sigNEGspace{-2.5em}       \def\TitleLineBIGskip{\\[0.75em]}

\newcounter{TotalSIGs}  \setcounter{TotalSIGs}{1}
\newcounter{SecondSIG}  \setcounter{SecondSIG}{0}
\newcounter{ThirdSIG}   \setcounter{ThirdSIG}{0}
\newcounter{FourthSIG}  \setcounter{FourthSIG}{0}
\newcounter{FifthSIG}   \setcounter{FifthSIG}{0}
\newcounter{SixthSIG}   \setcounter{SixthSIG}{0}
\newcounter{SeventhSIG} \setcounter{SeventhSIG}{0}
\newcounter{EighthSIG}  \setcounter{EighthSIG}{0}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 \newcommand{\secondmember}[1]{\TitleSign  
                                        \addtocounter{SecondSIG}{1}               % ....... Number of times the "\secondmember" command is used
                                        \addtocounter{TotalSIGs}{1}               % ....... Adds to Total Number of specified signature lines
                                        \ifthenelse{\value{SecondSIG} > 1}        % ....... Determines if "\secondmember" command is repeated
                                                   {\addtocounter{TotalSIGs}{-1}} % ....... Reduce Total if "\secondmember" command is repeated
                                                   {}                             % ....... Null "else" statement
                                        \def\addSecondMember{  \sigEXP  \\ \hline \\[\sigNEGspace] #1, Committee Member \EndSigLine } }
%--------------------------------------------------------------------------------------------------------------------------------------------------------
  \newcommand{\thirdmember}[1]{\TitleSign
                                        \addtocounter{ThirdSIG}{1}                % ....... Number of times the "\thirdmember" command is used
                                        \addtocounter{TotalSIGs}{1}               % ....... Adds to Total Number of specified signature lines           
                                        \ifthenelse{\value{ThirdSIG} > 1}         % ....... Determines if "\thirdmember" command is repeated  
                                                   {\addtocounter{TotalSIGs}{-1}} % ....... Reduce Total if "\thirdmember" command is repeated
                                                   {}                             % ....... Null "else" statement                                       
                                        \def\addThirdMember{   \sigEXP  \\ \hline \\[\sigNEGspace] #1, Committee Member \EndSigLine } }
%--------------------------------------------------------------------------------------------------------------------------------------------------------
 \newcommand{\fourthmember}[1]{\TitleSign
                                        \addtocounter{FourthSIG}{1}               % ....... Number of times the "\fourthmember" command is used
                                        \addtocounter{TotalSIGs}{1}               % ....... Adds to Total Number of specified signature lines           
                                        \ifthenelse{\value{FourthSIG} > 1}        % ....... Determines if "\fourthmember" command is repeated  
                                                   {\addtocounter{TotalSIGs}{-1}} % ....... Reduce Total if "\fourthmember" command is repeated
                                                   {}                             % ....... Null "else" statement                                       
                                        \def\addFourthMember{  \sigEXP  \\ \hline \\[\sigNEGspace] #1, Committee Member \EndSigLine } }
%--------------------------------------------------------------------------------------------------------------------------------------------------------
  \newcommand{\fifthmember}[1]{\TitleSign
                                        \addtocounter{FifthSIG}{1}                % ....... Number of times the "\fifthmember" command is used
                                        \addtocounter{TotalSIGs}{1}               % ....... Adds to Total Number of specified signature lines           
                                        \ifthenelse{\value{FifthSIG} > 1}         % ....... Determines if "\fifthmember" command is repeated  
                                                   {\addtocounter{TotalSIGs}{-1}} % ....... Reduce Total if "\fifthmember" command is repeated
                                                   {}                             % ....... Null "else" statement                                       
                                        \def\addFifthMember{   \sigEXP  \\ \hline \\[\sigNEGspace] #1, Committee Member \EndSigLine } }
%---------------------------------------------------------------------------------------------------------------------------------------------------------
  \newcommand{\sixthmember}[1]{\TitleSign
                                        \addtocounter{SixthSIG}{1}                % ....... Number of times the "\sixthmember" command is used
                                        \addtocounter{TotalSIGs}{1}               % ....... Adds to Total Number of specified signature lines           
                                        \ifthenelse{\value{SixthSIG} > 1}         % ....... Determines if "\sixthmember" command is repeated  
                                                   {\addtocounter{TotalSIGs}{-1}} % ....... Reduce Total if "\sixthmember" command is repeated
                                                   {}                             % ....... Null "else" statement                                       
                                        \def\addSixthMember{   \sigEXP  \\ \hline \\[\sigNEGspace] #1, Committee Member \EndSigLine } }
%---------------------------------------------------------------------------------------------------------------------------------------------------------
\newcommand{\seventhmember}[1]{\TitleSign 
                                        \addtocounter{SeventhSIG}{1}              % ....... Number of times the "\seventhmember" command is used
                                        \addtocounter{TotalSIGs}{1}               % ....... Adds to Total Number of specified signature lines           
                                        \ifthenelse{\value{SeventhSIG} > 1}       % ....... Determines if "\seventhmember" command is repeated  
                                                   {\addtocounter{TotalSIGs}{-1}} % ....... Reduce Total if "\seventhmember" command is repeated
                                                   {}                             % ....... Null "else" statement                                       
                                        \def\addSeventhMember{ \sigEXP  \\ \hline \\[\sigNEGspace] #1, Committee Member \EndSigLine } } 
%---------------------------------------------------------------------------------------------------------------------------------------------------------
 \newcommand{\eighthmember}[1]{\TitleSign  
                                        \addtocounter{EighthSIG}{1}               % ....... Number of times the "\eighthmember" command is used
                                        \addtocounter{TotalSIGs}{1}               % ....... Adds to Total Number of specified signature lines           
                                        \ifthenelse{\value{EighthSIG} > 1}        % ....... Determines if "\eighthmember" command is repeated  
                                                   {\addtocounter{TotalSIGs}{-1}} % ....... Reduce Total if "\eighthmember" command is repeated
                                                   {}                             % ....... Null "else" statement                                       
                                        \def\addEighthMember{  \sigEXP  \\ \hline \\[\sigNEGspace] #1, Committee Member \EndSigLine } }  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

\def\AddTitleSign{}
\def\TitleSign{\def\AddTitleSign{\addSecondMember  
                                 \addThirdMember   
                                 \addFourthMember 
                                 \addFifthMember    
                                 \addSixthMember    
                                 \addSeventhMember    
                                 \addEighthMember} }
                                      
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%              
    %%%  The general line spacing for the title page is     %%%
    %%%  controlled in the section "Assorted Stuff" below.  %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

\def\maketitle{\renewcommand{\thepage}{\roman{page}}  
\ifthenelse{\value{TotalSIGs} > 7} % ................................. Determines if there are 8 committee signatures (including the chair)
           {% ........................................................ Begins the "Then" to the first "If"
            \def\EndSigLine{\\[-0.75em]}              % ..........................................................................................................
            \def\EndChairSigLine{\\[-0.75em]}         % ...................... Spacing adjustments for when 8 committee member signature lines are      
            \def\SeperateDeanSigLine{\\[1.5em]}       % ...................... provided (including the chair).  Spacing is very `tight' in this case.
            \def\SeperateTitleFromSigLines{\\[1.5em]} % ..........................................................................................................
           }% ........................................................ Ends the "Then" to the first "If"
           {% ........................................................ Begins the "Else" to the first "If"
            \ifthenelse{\value{TotalSIGs} > 6} % ............................ Determines if there are 7 committee signatures (including the chair)
                       {% ........................................................ Begins the "Then" to the second "If"
                        \def\EndSigLine{\\[-0.5em]}             % ................................................................................................
                        \def\EndChairSigLine{\\[-0.5em]}        % ........................ Spacing adjustments for when 7 committee member signature lines are
                        \def\SeperateDeanSigLine{\\[1.75em]}    % ........................ provided (including the chair).  Spacing is `tight' in this case. 
                        \def\SeperateTitleFromSigLines{\\[2em]} % ................................................................................................
                       }% ........................................................ Ends the "Then" to the second "If"
                       {% ........................................................ Begins the "Else" to the second "If"
                        \def\EndSigLine{\\}                     % ................................................................................................
                        \def\EndChairSigLine{\\}                % ........................ Spacing settings for when 1 to 6 committee member signature lines are
                        \def\SeperateDeanSigLine{\\[2em]}       % ........................ provided (including the chair).  Spacing is `easy' in this case.     
                        \def\SeperateTitleFromSigLines{\\[2em]} % ................................................................................................
                       }% ........................................................ Ends the "Else" to the second "If"
           }% ........................................................ Ends the "Else" to the first "If"
\begin{titlepage} 
     \def\baselinestretch{4.2} 
     A \Treatise \TitleLineSkip
     entitled \TitleLineBIGskip  
     \ifthenelse{\equal{\@title}{}}
           { \fbox{\large\bf Error: insert title in $\backslash$title\{\}} }
           {\@title} \TitleLineBIGskip 
               %###  
               %###  The error command above covers the situation 
               %###  where the \title{} command is absent from the 
               %###  LaTeX file. 
               %###  
     by \TitleLineSkip 
     \ifthenelse{\equal{\@author}{}}
           { \fbox{\large\bf Error: insert name in $\backslash$author\{\}} }
           {\@author} \TitleLineBIGskip  
               %###  
               %###  The error command above covers the situation 
               %###  where the \author{} command is absent from  
               %###  the LaTeX file. 
               %###  
     Submitted to the Graduate Faculty as partial fulfillment of 
     the requirements for the \TitleLineSkip \DegreeLevel~Degree~in~\DegreeDiscipline
     \vfill     
     \hfill\begin{tabular}{l} \SeperateTitleFromSigLines \hline \\[\sigNEGspace]
              \Advisor, Committee Chair                       \EndChairSigLine
              \AddTitleSign                                   \SeperateDeanSigLine \hline \\[\sigNEGspace]
              \multicolumn{1}{c}{\Dean, \DeanTitle}           \\[-1em]
              \multicolumn{1}{c}{College of Graduate Studies} \\
           \end{tabular}  \vfill 
     The University of Toledo \\[-0.5em] 
     \AwardMonth~\AwardYear 
\end{titlepage}

\def\BlankSecondPage{\clearpage\mbox{} 
                     \thispagestyle{empty} 
                     \clearpage}                   
\SecondPage 
} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ENDS DEFINITION OF \maketitle

\def\SecondPage{\CopyRight}
\newcommand{\copyrightpage}[1]{\ifthenelse{\equal{#1}{} } %..................... 1st IF
                                  {\def\SecondPage{\CopyRight}} %............... THEN to 1st IF
                                  {\ifthenelse{\equal{#1}{no} } %............... Starts ELSE to 1st IF; starts 2nd IF
                                      {\def\SecondPage{\BlankSecondPage}} %..... THEN to 2nd IF
                                      {\def\SecondPage{\CopyRight}} %........... ELSE to 2nd IF
                                  } %........................................... Ends ELSE to 1st IF
                              } %............................................... Ends definition of \copyrightpage

%#########################################################################
%#########
%#########  COPYRIGHT PAGE
%#########  

\def\CopyRight{\clearpage\mbox{}
              \thispagestyle{empty} 
               \vfill
               \begin{singlespace}
               \noindent
               \begin{tabular}{p{.97\textwidth}}
               \multicolumn{1}{c}{Copyright~\AwardYear, \@author} 
               \\[1em] %%% PROVIDES 1 BLANK LINE BETWEEN MATERIAL
               This document is copyrighted material.
               Under copyright law, no parts of this
               document may be reproduced without the
               expressed permission of the author. 
               \end{tabular}
               \end{singlespace}
               %%% \mbox{} \\[-1em] \rule{0.5\textwidth}{0.5pt}
               \clearpage}

%#########################################################################
%#########
%#########  ABSTRACT PAGE
%#########  

\def\abstractpage{%%% BEGIN ABSTRACTPAGE DEFINITION
        \clearpage\addcontentsline{toc}{chapter}{Abstract}
        \begin{singlespace}
        \begin{center}{ %%% BEGINS TOP PORTION
        An Abstract of \TitleLineBIGskip
        \ifthenelse{\equal{\@title}{}}
                   { \fbox{\large\bf Error: insert title in $\backslash$title\{\}} }
                   {\@title} \TitleLineBIGskip
                       %###
                       %###  The error command above covers the situation 
                       %###  where the \title{} command is absent from the 
                       %###  LaTeX file.
                       %###
        by \TitleLineBIGskip
        \ifthenelse{\equal{\@author}{}}
                   { \fbox{\large\bf Error: insert name in $\backslash$author\{\}} }
                   {\@author} \\[1.2em]
                       %###
                       %###  The error command above covers the situation 
                       %###  where the \author{} command is absent from  
                       %###  the LaTeX file.
                       %###
        } %%% ENDS TOP PORTION        
        {  %%% BEGIN LOWER PORTION
        Submitted to the Graduate Faculty as partial fulfillment 
        of the requirements for the \TitleLineSkip
        \DegreeLevel~Degree~in~\DegreeDiscipline \TitleLineBIGskip
        The University of Toledo \\
        \AwardMonth~\AwardYear \\[2.25em]
        }  %%% END LOWER PORTION \\
        \end{center} 
        \end{singlespace}
        }  %%% END \abstractpage DEFINITION
\def\endabstractpage{\mbox{} \clearpage } 

%#########################################################################
%#########
%#########  DEDICATION PAGE
%######### 

\def\dedication{\thispagestyle{empty}\hbox{}\vspace{1.66in}}
\def\enddedication{\clearpage}

%#########################################################################
%#########
%#########  ACKNOWLEDGEMENT PAGE
%#########  

\def\acknowledgments{\chapter*{Acknowledgments}
                     \addcontentsline{toc}{chapter}{Acknowledgments}
                     \vspace{0.32in}}                     
\def\endacknowledgments{\clearpage}

%#########################################################################
%#########
%#########  LIST OF ABBREVIATIONS 
%######### 

\def\abbreviation#1#2{\setlength{\parskip}{-0.3em}\item[#1] #2 }
 
\newcommand{\abbreviationlabel}[1]{\mbox{\textrm{#1~}}\dotfill}
\newenvironment{listofabbreviations}
   {\chapter*{List of Abbreviations}
    \addcontentsline{toc}{chapter}{List of Abbreviations}
    \vspace{0.8em}  
    \begin{singlespace}  
          \begin{list}{}% 
          {\renewcommand{\makelabel}{\abbreviationlabel}%
           \setlength{\labelwidth}{2in}%
           \setlength{\leftmargin}{2.08in}%           
          }%
   }%
   {\end{list}\end{singlespace}}
                  
%#########################################################################
%#########
%#########  LIST OF SYMBOLS 
%######### 

\def\emblem#1#2{\setlength{\parskip}{-0.3em}\item[#1] #2 }
\def\emblemskip{\mbox{} \\[-1em]}
 
\newcommand{\symbollabel}[1]{\mbox{\textrm{#1~}}\dotfill}
\newenvironment{listofsymbols}
   {\chapter*{List of Symbols}
    \addcontentsline{toc}{chapter}{List of Symbols}
    \vspace{0.8em}  
    \begin{singlespace}  
          \begin{list}{}% 
          {\renewcommand{\makelabel}{\symbollabel}%
           \setlength{\labelwidth}{1in}%
           \setlength{\leftmargin}{1.08in}%           
          }%
   }%
   {\end{list}\end{singlespace}}
           
%#########################################################################
%#########
%#########  PREFACE PAGE
%######### 
 
\def\preface{\chapter*{Preface} 
             \addcontentsline{toc}{chapter}{Preface}
             \vspace{0.34in}}
\def\endpreface{\clearpage}

%#########################################################################
%#########  
%#########  Reference page
%#########  

\def\myreferences{\chapter*{References} 
                  \addcontentsline{toc}{chapter}{References}}

\def\referencelist#1{\ifthenelse{\equal{#1}{single}} %...................................... begins 1st "if"
                                {\vspace{0.12in}\begin{singlespace} %...................................... begins the "then" of 1st "if"
                                 \begin{description} %......................................
                                 \setlength{\parskip}{1em} %................................
                                 \setlength{\labelsep}{-1em} %..............................
                                 \def\endreferencelist{\end{description} %..................
                                                       \end{singlespace} } } %.............. ends the "then" of 1st "if"  
                                {\ifthenelse{\equal{#1}{double}} %.......................... begins the "else" of 1st "if" AND begins 2nd "if"
                                            {\vspace{2.4\topsep}\begin{description} %....... begins the "then" of 2nd "if"
                                             \setlength{\labelsep}{-1em} %..................
                                             \def\endreferencelist{\end{description}} } %... ends the "then" of 2nd "if"  
                                            {\begin{description} %.......................... begins the "else" of 2nd "if"
                                             \def\endreferencelist{\end{description}} } %... ends the "else" of 2nd "if"
                                  } %....................................................... ends 2nd "if"
                    } %..................................................................... ends 1st "if"                     
%#########################################################################
%#########
%#########  Assorted Stuff
%#########  

\def\titlepage{\newpage\centering
  \thispagestyle{empty}
  \parindent 0pt \parskip 10pt plus 1fil minus 1fil
  \def\baselinestretch{1.62}\@normalsize\vbox to \vsize\bgroup\vbox to 9in\bgroup}
\def\endtitlepage{\par\kern 0pt\egroup\vss\egroup\newpage}

\def\signature#1#2{\par\noindent#1\dotfill\null\\*
  {\raggedleft #2\par}}

\def\abstract{\small\def\baselinestretch{1}\@normalsize}
\def\endabstract{\par}

% If using the report style, use - instead of . in the figure number.
\@ifundefined{thechapter}{}{\def\thefigure{\thechapter-\arabic{figure}}}

\def\UTHESIS{{\sl U\kern-.095em\kern-.4667em\lower.7ex\hbox{Thesis}}}
\def\UT{{\rm U\kern-.05em\kern-.4667em\lower.7ex\hbox{T}}}

\def\BibTeX{{\rm B\kern-.05em{\sc i\kern-.025em b}\kern-.08em
    T\kern-.1667em\lower.7ex\hbox{E}\kern-.125emX}}

%#########################################################################
%#########  
%#########  Default Title and Author commands
%#########  
\title{\fbox{\large\bf Error: missing $\backslash$title\{\} command}}
\author{\fbox{\large\bf Error: missing $\backslash$author\{\} command}}
               %###  
               %###  The definition of "\maketitle" covers
               %###  the situation where the "\title{}" 
               %###   or "\author{}" command is blank (or
               %###  empty). 
               %###

%#########################################################################
%#########  
%#########  The captionformat command
%######### 
 
\newcommand{\captionformat}[1]{ \ifthenelse{\equal{#1}{align} }
                              { }
                              { \let\caption\hangcaption } }

%#########################################################################
%#########  
%#########  The following sets the headers and footers
%#########  for the text's pages:
%#########  

\newcommand{\makebody}{\clearpage
                       \renewcommand{\thepage}{\arabic{page}}  
                       \setcounter{page}{1}}

%#########################################################################
%#########
%#########  The following indents the first paragraph in a section
%#########

\typeout{------------> borrowing David Carlisle's indentfirst.sty}

\let\@afterindentfalse\@afterindenttrue
\@afterindenttrue

%#########################################################################
%#########  The following sets the section levels that
%#########  are enumerated and included in the
%#########  table of contents.
%#########

\setcounter{secnumdepth}{3}
\setcounter{tocdepth}{3}

%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################
%#########################################################################

\endinput
%% 
%% End of file `UThesis11b.cls'.

