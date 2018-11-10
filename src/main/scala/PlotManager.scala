// utility class for managing and combining GRACE plots into pdf files

package tools_

import types_._
import java.io._

case class PlotManager(name: Text, dir: Text = ".", startClean: Bool=true) {

  val run = Runtime.getRuntime

  val testDir = new File(dir)
  val outFile = new File(dir, s"$name.tex")

  if (startClean) cleanup() // clean out old or unneeded files

  val out = new PrintWriter(outFile)

  private def cleanup(numFiles: Int=2) {

    if (numFiles > 1) removeFiles(".dat")
    removeFiles(".ps")
    removeFiles(".pdf")
    removeFiles(".tex")
    removeFiles(".log")
    removeFiles(".aux")
    }

  private def removeFiles(txt: Text) {

    val filter = new FilenameFilter {
      override def accept(dir: File, file: String) = {
        file.startsWith(name) && (file.endsWith(txt)) && file != s"$name.pdf"
        }
      }

    val files = testDir.listFiles(filter).mkString(" ")
    run.exec("rm -f " + files).waitFor // use File.delete instead?
    ()
    }

  def combinePlots(display: Bool=true, save: Bool=false) {

    val files = testDir.listFiles.toList.map(_.getName)
      .filter(_.startsWith(name)).filter(_.endsWith(".dat")).sorted

    printLatexHeader

    for (f <- files) out.println("\\plot{" + f.replace(".dat",".pdf") + "}")
    out.println("\n\\end{document}")
    out.close

    for (file <- files.par) {
      val psfile = file.replace(".dat",".ps")
      run.exec(s"gracebat -printfile $psfile $file").waitFor
      run.exec(s"ps2pdf $psfile").waitFor
      }

    run.exec(s"pdflatex $name").waitFor

    if (display) displayPlots

    if (not(save)) cleanup(files.length)
    }

  def displayPlots() {

    val runningLocally = System.getenv("SSH_CLIENT") == null
    if (runningLocally) run.exec(s"acroread $name.pdf")
    ()
    }

  private def printLatexHeader() {

    out.println("\\documentclass[landscape]{slides}")
    out.println("\\usepackage{graphicx}")
    out.println("\\usepackage{epstopdf}")
    out.println("")
    out.println("\\setlength{\\textwidth}{11in}")
    out.println("\\setlength{\\textheight}{8.5in}")
    out.println("\\setlength{\\topmargin}{-1.5in}")
    out.println("\\setlength{\\oddsidemargin}{-1in}")
    out.println("\\setlength{\\evensidemargin}{\\oddsidemargin}")
    out.println("")
    out.println("\\newcommand{\\plot}[1]{")
    out.println("  \\begin{slide}\\begin{center}")
    out.println("    \\includegraphics[width=10.5in,angle=0]{#1}")
    out.println("  \\end{center}\\end{slide}}")
    out.println("")
    out.println("\\begin{document}\n")
    }
  }
