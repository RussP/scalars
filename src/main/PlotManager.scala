// utility class for managing and combining GRACE plots into pdf files

package tools_

import types_._
import java.io._

import collection.parallel.CollectionConverters._

case class PlotManager(name: Text, dir: Text=".", startClean: Bool=true) {

  val run = Runtime.getRuntime

  val testDir = new File(dir)
  val texFile = new File(dir, s"$name.tex")

  if (startClean) cleanup() // clean out old or unneeded files

  val out = new PrintWriter(texFile)

  private def cleanup(numFiles: Int=2) = {

    def delete1(txt: Text) = new File(dir, name + txt).delete

    delete1(".aux") // delete file created by latex
    delete1(".log") // ditto
    delete1(".tex") // ditto

    deleteFiles(".ps") // delete in termediate postscript files
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

  private def replaceAtEnd(txt: Text, ext1: Text, ext2: Text): Text =
    // used to change file name extension
    if (not(txt.endsWith(ext1))) txt else
      txt.substring(0, txt.length - ext1.length) + ext2

  }
