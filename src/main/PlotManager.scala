// utility class for managing and combining GRACE plots into pdf files

package tools_

import types_._
import java.io._
import scala.sys.process.Process
import collection.parallel.CollectionConverters._

case class PlotManager(name: Text, dir: Text=".", startClean: Bool=true) {

  val testDir = new File(dir)
  val texFile = new File(dir, s"$name.tex")

  if (startClean) cleanup() // clean out old or unneeded files

  val out = new PrintWriter(texFile)

  private def cleanup(numFiles: Int=2) = {

    def delete1(txt: Text) = new File(dir, name + txt).delete

    delete1(".aux") // delete file created by latex
    delete1(".log") // ditto
    delete1(".tex") // ditto

    deleteFiles(".ps") // delete intermediate postscript files
    deleteFiles(".pdf") // delete individual pdf files for each plot
    if (numFiles > 1) deleteFiles(".dat")
    ()
    }

  private def deleteFiles(ext: Text) = {

    val filter = new FilenameFilter {
      override def accept(dir: File, file: String) =
        file.startsWith(name) && (file.endsWith(ext)) && file != s"$name.pdf"
      }

    for (file <- testDir.listFiles(filter)) file.delete
    }

  def packagePlots(title: Text="", author: Text="", intro: Text="",
    showDate: Bool=false, margin: Text="1in", parsep: Text="0.3in",
    display: Bool=true, save: Bool=false) =

    combinePlots(title, author, intro, showDate, margin, parsep,
        display, save)

  def combinePlots(title: Text="", author: Text="", intro: Text="",
    showDate: Bool=false, margin: Text="1in", parsep: Text="0.3in",
    display: Bool=true, save: Bool=false): Unit = {

    val utag = "-utag1zqp937" // unique tag to avoid name clash

    val fileNames = {

      val fileNames0 = testDir.listFiles.toList.map(_.getName)
        .filter(_.startsWith(name)).filter(_.endsWith(".dat"))

      // The following line compensates for an apparent bug in gracebat
      // that was preventing the filling of "boxes" (rectangles) on the plot:
      for (f <- fileNames0) Process(s"sed -i '1i@version 50125' $f").!

      val fileNames1 = fileNames0.map(x=>replaceAtEnd(x,".dat",".pdf")).sorted

      if (fileNames1.isEmpty) { cleanup(); return }

      val sameName = s"$name.pdf"
      val indx = fileNames1.indexOf(sameName)

      if (indx >= 0) { // avoid name clash with output file name
        val newName = s"$name$utag.pdf"
        val sameFile = new File(sameName)
        val newFile = new File(newName)
        sameFile.renameTo(newFile)
        fileNames1.updated(indx, newName)
        }

      else fileNames1
      }

    printLatexHeader(title, author, intro, showDate, margin, parsep)

    for (fileName <- fileNames) if (fileName.count(_ == '.') > 1)
      // pdflatex will choke on file name if it has more than one "dot" (".")
      throw new RuntimeException(s"\n$fileName has more than one dot\n")

    for (fileName <- fileNames) out.println(s"\\plot{$fileName}")
    out.println("\n\\end{document}")
    out.close

    for (fileName <- fileNames.par) { // ".par" for parallel processing
      val datFile = replaceAtEnd(fileName,".pdf",".dat").replace(utag,"")
      val psFile = replaceAtEnd(fileName,".pdf",".ps")
      Process(s"gracebat -printfile $psFile $datFile").!
      Process(s"ps2pdf $psFile").!
      }

    val out1 = System.out
    if (not(save)) System.setOut(new PrintStream(nullFile))

    Process(s"pdflatex $name").!

    if (display) displayPlots

    if (save) deleteFiles(".ps") else { // save all temp files except .ps
      cleanup(fileNames.length) // delete all but the final pdf output file
      System.setOut(out1) // reset standard output
      }
    }

  def displayPlots = {
    val runningLocally = System.getenv("SSH_CLIENT") == null
    val command = s"acroread $name.pdf"
    //val command = s"evince --presentation $name.pdf"
    if (runningLocally) Runtime.getRuntime.exec(command)
    }

  private def printLatexHeader(title: Text, author: Text, intro: Text,
    showDate: Bool, margin: Text, parsep: Text) = {

    val (m1, p1) = (margin, parsep)

    out.println("\\documentclass[landscape]{slides}")
    out.println("\\usepackage{graphicx}")
    out.println("\\usepackage{epstopdf}\n")

    out.println("\\renewenvironment{quote}")
    out.println("  {\\small\\list{}")
    out.println(s"  {\\rightmargin=$m1 \\leftmargin=$m1 \\parsep=$p1}%")
    out.println("  \\item\\relax}{\\endlist}\n")

    out.println("\\newcommand{\\plot}[1]{")
    out.println("  \\begin{slide}\\begin{center}")
    out.println("    \\includegraphics[width=10.5in,angle=0]{#1}")
    out.println("  \\end{center}\\end{slide}}\n")

    out.println("\\setlength{\\textwidth}{11in}")
    out.println("\\setlength{\\textheight}{8.5in}")
    out.println("\\setlength{\\topmargin}{-1.5in}")
    out.println("\\setlength{\\oddsidemargin}{-1in}")
    out.println("\\setlength{\\evensidemargin}{\\oddsidemargin}")

    if (title.nonEmpty) out.println(s"\n\\title{\\textbf{$title}}")
    if (author.nonEmpty) out.println(s"\\author{$author}")
    if (not(showDate)) out.println("\\date{}")

    out.println("\n\\begin{document}\n")

    if (title.nonEmpty) out.println("\\maketitle\n")

    if (intro.nonEmpty) {
      out.println("\\begin{quote}\n")
      out.println(intro) // optional explanatory text
      out.println("\n\\end{quote}\n")
      }
    }

  private def replaceAtEnd(txt: Text, ext1: Text, ext2: Text): Text =
    // used to change file name extension
    if (not(txt.endsWith(ext1))) txt else
      txt.substring(0, txt.length - ext1.length) + ext2

  }
