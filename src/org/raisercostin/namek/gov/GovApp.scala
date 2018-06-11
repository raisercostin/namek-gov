package org.raisercostin.namek.gov

object GovApp {

  def escape(text:String) =
    decompose(if(!isSimpleDotId(text)) s""""${text.replace("\n","\\n")}"""" else text)

  def decompose(s: String): String = java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "")

  //See An ID is one of the following: http://www.graphviz.org/doc/info/lang.html
  def isSimpleDotId(text:String):Boolean =
    //text.matches("""^[a-zA-Z\0200-\0377_][a-zA-Z\0200-\0377_0-9]*$""")
    text.matches("""^[a-zA-Z_0-9]*$""")

  trait WithAttributes{self=>
    def attributes:Map[String,String] = attr
    var attr:Map[String,String] = Map()
    def addAttribute(src:String,dest:String):self.type ={
      attr += src -> dest
      self
    }
    def addAttributes(args: (String, Any)*):self.type = addAttributes2(args.toMap.mapValues(_.toString))
    private def addAttributes2(map: Map[String, String]):self.type = {
      attr ++= map
      self
    }
    //see https://en.wikipedia.org/wiki/DOT_(graph_description_language)
    def toDot:String = if(attributes.isEmpty) "" else s""" [${attributes.map(x=>escape(x._1)+"="+escape(x._2)).mkString(",")}]"""
  }

  case class Node(id:String) extends WithAttributes {
    override def toDot:String = s"$id${super.toDot}"
  }
  case class Edge(src:Node, dst:Node) extends WithAttributes {
    override def attributes:Map[String,String] =
      if(attr.contains("label"))
        attr
      else
        attr.get("kind").map(x=>attr + ("label" -> x)).getOrElse(attr)
    override def toDot:String = s"""${src.id}->${dst.id}${super.toDot}"""
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
    def findOrCreateNodeWithParent(nodeId: String, parent:Node):Node = {
      val node = findOrCreateNode(nodeId)
      addEdge(parent.id,nodeId).addAttribute("label","include").addAttribute("kind","include")
      node
    }

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
        |  node [style=filled, color="0.650 0.200 1.000"];
        |  rank="sink"
        |  rankdir=TB
        |  ${n.map(_.toDot).mkString("\n  ")}
        |  ${e.map(_.toDot).mkString("\n  ")}
        |}
      """.stripMargin
  }
  class NodeBuilder(graph:Graph) extends Dynamic {
    def selectDynamic(name:String): NodeBuilder2 = new NodeBuilder2(graph,graph.findOrCreateNode(name))
    def applyDynamicNamed(name:String)(args: (String, Any)*):NodeBuilder2 = {
      val node = graph.findOrCreateNode(name)
      node.addAttributes(args:_*)
      new NodeBuilder2(graph,node)
    }
  }
  class NodeBuilder2(graph:Graph, parent:Node) extends Dynamic {
    def selectDynamic(name:String): NodeBuilder2 = new NodeBuilder2(graph,graph.findOrCreateNodeWithParent(name,parent))
    def applyDynamicNamed(name:String)(args: (String, Any)*):NodeBuilder2 = {
      val node = graph.findOrCreateNodeWithParent(name,parent)
      node.addAttributes(args:_*)
      new NodeBuilder2(graph,node)
    }
  }
  class EdgeBuilder(graph:Graph) extends Dynamic {
    def selectDynamic(src:String): EdgeBuilder2 = new EdgeBuilder2(graph,src)
  }
  class EdgeBuilder2(graph:Graph, src:String) extends Dynamic {
    def selectDynamic(dest:String): EdgeBuilder3 = {val edge = graph.addEdge(src,dest);new EdgeBuilder3(graph,edge)}
  }
  class EdgeBuilder3(graph:Graph, edge:Edge) extends Dynamic {
    def applyDynamic(name:String)(value:String*): EdgeBuilder3 = {
      if(name!="attributes")
        edge.addAttribute("kind",name)
      value.headOption.map{edge.addAttribute(name,_)}.getOrElse();
      this
    }
    def applyDynamicNamed(name:String)(args: (String, Any)*):EdgeBuilder3 = {
      if(name!="attributes")
        edge.addAttribute("kind",name)
      edge.addAttributes(args:_*)
      this
    }
  }

  def main(args: Array[String]): Unit = {
    println("//view at http://www.webgraphviz.com/")

    val graph = new Graph()
    graph.nodes.international.cedo(label="CEDO\nCurtea European a Drepturilor Omului")

    graph.nodes.puteri.judiciar(label="judiciar (aplică și interpretează legile)")
    graph.nodes.puteri.legislativ
    graph.nodes.puteri.executiv
    graph.nodes.puteri.presa
    graph.nodes.puteri.cetateni
    graph.edge.cetateni.asociatii.asociaza()

    graph.nodes.judiciar.judecatori
    graph.nodes.magistrati.judecatori
    graph.nodes.judecatori.ccr(label="CCR\nCurtea Constitutionala a Romaniei\n9 judecatori\n9 ani",mandat="9 ani")
    graph.nodes.judecatori.iccj(label="ÎCCJ\nÎnalta Curte de Casație și Justiție.")

    graph.nodes.magistrati.procurori
    graph.nodes.procurori.dna(label="DNA\nDepartamentul National Anticoruptie\n(fost PNA - Parchetul National Anticoruptie)")
    graph.nodes.procurori.piccj(label="PICCJ\nPICCJ - Parchetul de pe langa Înalta Curte de Casație și Justiție.)")

    graph.nodes.asociatii.partide
    graph.nodes.asociatii.sindicate
    graph.nodes.asociatii.patronate
    graph.nodes.asociatii.asociatiiProfesionale(label="Asociatii Profesionale")

    graph.edge.csm.magistrati.propune()
    graph.edge.cedo.iccj.control(label="corectie decizii")

    graph.edge.piccj.iccj.deserveste()

    graph.nodes.by(label="by raisercostin & alexugoku (c) 2018")
    println(graph.toDot)
  }
}
