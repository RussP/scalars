// utility class for managing and combining GRACE plots into pdf files

package tools_

import types_._
import java.io._

import collection.parallel.CollectionConverters._

case class PlotManager(name: Text, dir: Text=".", startClean: Bool=true) {

  val run = Runtime.getRuntime

  val testDir = new File(dir)
  val outFile = new File(dir, s"$name.tex")

  if (startClean) cleanup() // clean out old or unneeded files

  val out = new PrintWriter(outFile)

  private def cleanup(numFiles: Int=2) = {

    removeFiles(".ps")
    removeFiles(".tex")
    removeFiles(".log")
    removeFiles(".aux")
    removeFiles(".pdf")

    if (numFiles > 1) removeFiles(".dat")
    }

  private def removeFiles(txt: Text) = {

    val filter = new FilenameFilter {
      override def accept(dir: File, file: String) = {
        file.startsWith(name) && (file.endsWith(txt)) && file != s"$name.pdf"
        }
      }

    val files = testDir.listFiles(filter).mkString(" ")

    run.exec(s"rm -f $files").waitFor // use File.delete instead?
    ()
    }

  def combinePlots(display: Bool=true, save: Bool=false): Unit = {

    val utag = "-utag1zqp" // unique tag to avoid name clash

    val files = {

      val files1 = testDir.listFiles.toList.map(_.getName)
        .filter(_.startsWith(name)).filter(_.endsWith(".dat"))
        .map(x=>replaceAtEnd(x,".dat",".pdf")).sorted

      if (files1.isEmpty) { cleanup(); return }

      val sameName = s"$name.pdf"
      val indx = files1.indexOf(sameName)

      if (indx >= 0) { // avoid name clash with output file name
        val newName = s"$name$utag.pdf"
        val sameFile = new File(sameName)
        val newFile = new File(newName)
        sameFile.renameTo(newFile)
        files1.updated(indx, newName)
        }

      else files1
      }

    printLatexHeader

    for (file <- files) out.println(s"\\plot{$file}")
    out.println("\n\\end{document}")
    out.close

    for (file <- files.par) { // ".par" for parallel processing
      val datFile = replaceAtEnd(file,".pdf",".dat").replace(utag,"")
      val psFile = replaceAtEnd(file,".pdf",".ps")
      run.exec(s"gracebat -printfile $psFile $datFile").waitFor
      run.exec(s"ps2pdf $psFile").waitFor
      }

    run.exec(s"pdflatex $name").waitFor

    if (display) displayPlots
    if (not(save)) cleanup(files.length)
    }

  def displayPlots() = {
    val runningLocally = System.getenv("SSH_CLIENT") == null
    if (runningLocally) run.exec(s"acroread $name.pdf")
    ()
    }

  private def printLatexHeader() = {

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

  def replaceAtEnd(txt: Text, ext1: Text, ext2: Text): Text =
    // used to change file name extension
    if (not(txt.endsWith(ext1))) txt else
      txt.substring(0, txt.length - ext1.length) + ext2

  }
