// utility class for managing and combining GRACE plots into pdf files

package tools_

import types_._

import java.io._

case class PlotManager(name: Text, dir: Text = ".", startClean: Bool=true) {

  val run = Runtime.getRuntime

  val testDir = new File(dir)
  val outFile = new File(dir, name + ".tex")

  if (startClean) {
    removeFiles(".ps") // clean out old files (this must be here!)
    removeFiles(".dat")
    removeFiles(".pdf")
    }

  val out = new PrintWriter(outFile)

  private def removeFiles(txt: Text) {

    val filter = new FilenameFilter {
      override def accept(dir: File, file: String) =
        file.startsWith(name) && (file.endsWith(txt))
      }

    val files = testDir.listFiles(filter).mkString(" ")
    run.exec("rm -f " + files).waitFor // use File.delete instead?
    ()
    }

  private def latexToPDFplotsXXX() = {

    val namex = name + "-randomstuffxxx9871" // avoid name clash

    run.exec(s"latex $name").waitFor
    run.exec(s"dvips -o $namex.ps $name").waitFor // stuck on this!
    //Thread.sleep(10000) // use pause instead to avoid hanging
    run.exec(s"ps2pdf $namex.ps $name.pdf").waitFor
    run.exec(s"rm -f $name.tex $name.log $name.aux $name.dvi $namex.ps")

    removeFiles(".ps")
    }

  private def latexToPDFplotsZZZ() = {

    import scala.sys.process._

    val namex = name + "-randomstuffxxx9871" // avoid name clash

    s"latex --interaction=batchmode $name".!
    s"dvips -q -o $namex.ps $name".!
    s"ps2pdf $namex.ps $name.pdf".!
    s"rm -f $name.tex $name.log $name.aux $name.dvi $namex.ps".!

    removeFiles(".ps")
    }

  private def latexToPDFplots() = {

    import scala.sys.process._

    s"tex2pdf1 $name".!

    removeFiles(".ps")
    //removeFiles(".tex")
    }

  def combinePlots(display: Bool=true, save: Bool=false) {

    val files = testDir.listFiles.toList.map(_.getName)
      .filter(_.startsWith(name)).filter(_.endsWith(".dat")).sorted

    printLatexHeader

    for (f <- files) out.println("\\plot{" + f.replace(".dat",".ps") + "}")
    out.println("\n\\end{document}")
    out.close

    for (file <- files.par) {
      val psfile = file.replace(".dat",".ps")
      run.exec(s"gracebat -printfile $psfile $file")
      }

    latexToPDFplots

    if (display) displayPlots

    if (save) return

    if (files.length > 1) removeFiles(".dat")
    removeFiles(".tex")
    }

  private def printLatexHeader() {

    out.println("\\documentclass{slides}")
    out.println("\\usepackage[dvips]{graphics}")
    out.println("")
    out.println("\\setlength{\\topmargin}{6in}")
    out.println("\\setlength{\\oddsidemargin}{-0.9in}")
    out.println("\\setlength{\\evensidemargin}{\\oddsidemargin}")
    out.println("")
    out.println("\\newcommand{\\plot}[1]{\\begin{slide}")
    out.println("    \\includegraphics[1,1]{#1}\\end{slide}}")
    out.println("")
    out.println("\\begin{document}\n")
    }

  def displayPlots() {

    val runningLocally = System.getenv("SSH_CLIENT") == null
    if (runningLocally) run.exec(s"acroread $name.pdf")
    ()
    }
  }
