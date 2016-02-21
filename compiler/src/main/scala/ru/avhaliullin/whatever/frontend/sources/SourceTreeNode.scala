package ru.avhaliullin.whatever.frontend.sources

import java.io.File

/**
  * @author avhaliullin
  */
sealed trait SourceTreeNode[T] {
  def name: String

  def file: File

  def map[R](f: (SourceTreeNode.SrcFile[T]) => R): SourceTreeNode[R]
}

object SourceTreeNode {

  case class SrcDirectory[T](name: String, children: Seq[SourceTreeNode[T]], file: File) extends SourceTreeNode[T] {
    override def map[R](f: (SrcFile[T]) => R): SourceTreeNode[R] = copy(children = children.map(_.map(f)))
  }

  case class SrcFile[T](name: String, file: File, payload: T) extends SourceTreeNode[T] {
    override def map[R](f: (SrcFile[T]) => R): SourceTreeNode[R] = copy(payload = f(this))

    def fullPath: String = file.getAbsolutePath
  }

  private val ext = ".we"

  private def listDirSources(root: File): Seq[SourceTreeNode[Unit]] = {
    root.listFiles().toSeq.collect {
      case dir if dir.isDirectory => SrcDirectory[Unit](dir.getName, listDirSources(dir), dir)
      case file if file.getName.endsWith(ext) => SrcFile[Unit](file.getName.dropRight(ext.length), file, ())
    }
  }

  def findSources(root: File): SourceTree[Unit] = {
    SourceTree(listDirSources(root))
  }
}

case class SourceTree[T](sources: Seq[SourceTreeNode[T]]) {
  def map[R](f: (SourceTreeNode.SrcFile[T]) => R): SourceTree[R] = SourceTree(sources.map(_.map(f)))
}