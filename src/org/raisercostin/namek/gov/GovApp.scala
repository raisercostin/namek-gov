package org.raisercostin.namek.gov

object GovApp {
  case class Node(id:String){
    def toDot = s"$id"
  }
  case class Edge(src:Node, dst:Node, var attributes:Map[String,String] = Map()){
    def addAttribute(src:String,dest:String*):Edge ={
      attributes += src -> dest.head
      this
    }
    def toDot = s"""${src.toDot}->${dst.toDot} [${attributes.map(x=>x._1+"="+x._2).mkString(",")}]"""
  }

  import scala.language.dynamics
  case class Graph(var n:Set[Node]=Set(),var e:Seq[Edge]=Seq()) {
    val nodes = new NodeBuilder(this)
    val edge = new EdgeBuilder(this)
    def addEdge(src:String,dest:String):Edge ={
      val edge = new Edge(findOrCreateNode(src),findOrCreateNode(dest))
      e = e :+ edge
      edge
    }
    def findOrCreateNode(nodeId: String):Node =
      n.find(_.id==nodeId).getOrElse(addNode(nodeId))
    private def addNode(src:String):Node = {
      val node = Node(src)
      n += node
      node
    }

    def toDot =
      s"""
        |digraph G {
        |  compound=true;
        |  ratio = fill;
        |	node [style=filled, color="0.650 0.200 1.000"];
        |  rank="sink"
        |  rankdir=TB
        |  ${e.map(_.toDot).mkString("\n")}
        |  }
      """.stripMargin
  }
  class NodeBuilder(graph:Graph) extends Dynamic {
    def selectDynamic(name:String): NodeBuilder = {graph.findOrCreateNode(name);this}
  }
  class EdgeBuilder(graph:Graph) extends Dynamic {
    def selectDynamic(src:String): EdgeBuilder2 = new EdgeBuilder2(graph,src)
  }
  class EdgeBuilder2(graph:Graph, src:String) extends Dynamic {
    def selectDynamic(dest:String): EdgeBuilder3 = {val edge = graph.addEdge(src,dest);new EdgeBuilder3(graph,edge)}
  }
  class EdgeBuilder3(graph:Graph, edge:Edge) extends Dynamic {
    def applyDynamic(name:String)(params:String*): EdgeBuilder3 = {edge.addAttribute(name,params:_*);this}
  }

  def main(args: Array[String]): Unit = {
    println("hi")
    val graph = new Graph()
    graph.nodes.cedo.cedo2.cedo3
    graph.edge.cedo.cedo2.description("gigi")
    graph.edge.cedo.cedo2.label("aaa").constraint("gigi")
    println(graph.toDot)
  }
}

//define nodes
//define edges
//define subnodes
//define hiper edges (conditii)
//control type