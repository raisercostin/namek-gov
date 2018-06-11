package org.raisercostin.namek.gov

object GovApp {

  def escape(text: String) =
    decompose(if (!isSimpleDotId(text)) s""""${text.replaceAll("[\n\r]+", "\\\\n")}"""" else text)

  def decompose(s: String): String = java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "")

  //See An ID is one of the following: http://www.graphviz.org/doc/info/lang.html
  def isSimpleDotId(text: String): Boolean =
  //text.matches("""^[a-zA-Z\0200-\0377_][a-zA-Z\0200-\0377_0-9]*$""")
    text.matches("""^[a-zA-Z_0-9]*$""")

  trait WithAttributes {
    self =>
    def attributes: Map[String, String] = attr

    var attr: Map[String, String] = Map()

    def addAttribute(src: String, dest: String): self.type = {
      attr += src -> dest
      self
    }

    def addAttributes(args: (String, Any)*): self.type = addAttributes2(args.toMap.mapValues(_.toString))

    private def addAttributes2(map: Map[String, String]): self.type = {
      attr ++= map
      self
    }

    //see https://en.wikipedia.org/wiki/DOT_(graph_description_language)
    def toDot: String = if (attributes.isEmpty) "" else s""" [${attributes.map(x => escape(x._1) + "=" + escape(x._2)).mkString(",")}]"""
  }

  case class Node(id: String) extends WithAttributes {
    override def toDot: String = s"$id${super.toDot}"
  }

  case class Edge(src: Node, dst: Node) extends WithAttributes {
    override def attributes: Map[String, String] =
      if (attr.contains("label"))
        attr
      else
        attr.get("kind").map(x => attr + ("label" -> x)).getOrElse(attr)

    override def toDot: String = s"""${src.id}->${dst.id}${super.toDot}"""
  }

  import scala.language.dynamics

  case class Graph(var n: Set[Node] = Set(), var e: Seq[Edge] = Seq()) {
    val nodes = new NodeBuilder(this)
    val edge = new EdgeBuilder(this)

    def addEdge(src: String, dst: String): Edge =
      e.find(x => x.src.id == src && x.dst.id == dst).getOrElse {
        val edge = new Edge(findOrCreateNode(src), findOrCreateNode(dst))
        e = e :+ edge
        edge
      }

    def findOrCreateNode(nodeId: String): Node =
      n.find(_.id == nodeId).getOrElse(addNode(nodeId))

    def findOrCreateNodeWithParent(nodeId: String, parent: Node): Node = {
      val node = findOrCreateNode(nodeId)
      addEdge(parent.id, nodeId).addAttribute("label", "include").addAttribute("kind", "include")
      node
    }

    private def addNode(src: String): Node = {
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

  class NodeBuilder(graph: Graph) extends Dynamic {
    def selectDynamic(name: String): NodeBuilder2 = new NodeBuilder2(graph, graph.findOrCreateNode(name))

    def applyDynamicNamed(name: String)(args: (String, Any)*): NodeBuilder2 = {
      val node = graph.findOrCreateNode(name)
      node.addAttributes(args: _*)
      new NodeBuilder2(graph, node)
    }
  }

  class NodeBuilder2(graph: Graph, parent: Node) extends Dynamic {
    def selectDynamic(name: String): NodeBuilder2 = new NodeBuilder2(graph, graph.findOrCreateNodeWithParent(name, parent))

    def applyDynamicNamed(name: String)(args: (String, Any)*): NodeBuilder2 = {
      val node = graph.findOrCreateNodeWithParent(name, parent)
      node.addAttributes(args: _*)
      new NodeBuilder2(graph, node)
    }
  }

  class EdgeBuilder(graph: Graph) extends Dynamic {
    def selectDynamic(src: String): EdgeBuilder2 = new EdgeBuilder2(graph, src)
  }

  class EdgeBuilder2(graph: Graph, src: String) extends Dynamic {
    def selectDynamic(dest: String): EdgeBuilder3 = {
      val edge = graph.addEdge(src, dest); new EdgeBuilder3(graph, edge)
    }
  }

  class EdgeBuilder3(graph: Graph, edge: Edge) extends Dynamic {
    def applyDynamic(name: String)(value: String*): EdgeBuilder3 = {
      if (name != "attributes")
        edge.addAttribute("kind", name)
      value.headOption.map {
        edge.addAttribute(name, _)
      }.getOrElse();
      this
    }

    def applyDynamicNamed(name: String)(args: (String, Any)*): EdgeBuilder3 = {
      if (name != "attributes")
        edge.addAttribute("kind", name)
      edge.addAttributes(args: _*)
      this
    }
  }

  def main(args: Array[String]): Unit = {
    println("//view at http://www.webgraphviz.com/")
    println("//various engine and formats at https://dreampuf.github.io/GraphvizOnline/")
    val graph = new Graph()
    graph.nodes.international.cedo(label = "CEDO\nCurtea European a Drepturilor Omului")

    graph.nodes.puteri.legislativ
    graph.nodes.puteri.legislativ.parlament
    graph.nodes.puteri.legislativ.parlament.deputati.presedinteDeputati(label = "Preşedintele Camerei Deputaţilor")
    graph.nodes.puteri.legislativ.parlament.senatori.presedinteSenat(label = "Preşedintele Senatului")
    graph.nodes.puteri.legislativ.legislatie.constitutie
    graph.nodes.puteri.legislativ.legislatie.legi.legiParlament(URL = "http://www.constitutiaromaniei.ro/art-73-categorii-de-legi/#1")
    graph.nodes.puteri.legislativ.legislatie.legi.legiParlament.legiConstitutionale(label = "Legi revizuire constitutie")
    graph.nodes.puteri.legislativ.legislatie.legi.legiParlament.legiOrganice
    graph.nodes.puteri.legislativ.legislatie.legi.legiParlament.legiOrdinare(label = "Legi Ordinare (restul)")
    graph.nodes.puteri.legislativ.legislatie.regulamente

    graph.edge.parlament.legiParlament.adopta()

    graph.nodes.legiOrganice(label = "Legi Organice", tooltip =
      """Legi de mare importanta. Prin lege organică se reglementează:
        |a) sistemul electoral; organizarea şi funcţionarea Autorităţii Electorale Permanente;
        |b) organizarea, funcţionarea şi finanţarea partidelor politice;
        |c) statutul deputaţilor şi al senatorilor, stabilirea indemnizaţiei şi a celorlalte drepturi ale acestora;
        |d) organizarea şi desfăşurarea referendumului;
        |e) organizarea Guvernului şi a Consiliului Suprem de Apărare a Ţării;
        |f) regimul stării de mobilizare parţială sau totală a forţelor armate şi al stării de război;
        |g) regimul stării de asediu şi al stării de urgenţă;
        |h) infracţiunile, pedepsele şi regimul executării acestora;
        |i) acordarea amnistiei sau a graţierii colective;
        |j) statutul funcţionarilor publici;
        |k) contenciosul administrativ;
        |l) organizarea şi funcţionarea Consiliului Superior al Magistraturii, a instanţelor judecătoreşti, a Ministerului Public şi a Curţii de Conturi;
        |m) regimul juridic general al proprietăţii şi al moştenirii;
        |n) organizarea generală a învăţământului;
        |o) organizarea administraţiei publice locale, a teritoriului, precum şi regimul general privind autonomia locală;
        |p) regimul general privind raporturile de muncă, sindicatele, patronatele şi protecţia socială;
        |r) statutul minorităţilor naţionale din România;
        |s) regimul general al cultelor;
        |t) celelalte domenii pentru care în Constituţie se prevede adoptarea de legi organice.""".stripMargin)

    graph.nodes.puteri.executiv(tooltip="(guvernează - implementarea legilor în practică și cu administrarea birocrației de stat)")
    graph.nodes.puteri.executiv.presedinte(URL="http://www.contributors.ro/dezbatere/despre-o-inertie-politica-si-constitutionala-presedintele-romaniei-parte-a-puterii-executive/")
    graph.nodes.puteri.executiv.guvern(tooltip="Guvernul însă poate propune legi spre aprobare de către legislativ.")
    graph.nodes.puteri.executiv.guvern.ministri
    graph.nodes.puteri.executiv.guvern.ministri.primMinistru
    graph.nodes.puteri.executiv.guvern.ministri.ministrulAparariiNationale
    graph.nodes.puteri.executiv.guvern.ministri.ministrulAfacerilorInterne
    graph.nodes.puteri.executiv.guvern.ministri.ministrulAfacerilorExterne
    graph.nodes.puteri.executiv.guvern.ministri.ministrulJustitiei
    graph.nodes.puteri.executiv.guvern.ministri.ministrulEconomiei
    graph.nodes.puteri.executiv.guvern.ministri.ministrulFinantelorPublice

    graph.nodes.puteri.executiv.guvern.minister.mapn
    graph.nodes.puteri.executiv.guvern.minister.mai
    graph.nodes.puteri.executiv.guvern.minister.mae
    graph.nodes.puteri.executiv.guvern.minister.mj
    graph.nodes.puteri.executiv.guvern.minister.mfp
    graph.nodes.puteri.executiv.guvern.minister.me
    graph.nodes.puteri.executiv.guvern.minister.ms
    graph.nodes.puteri.executiv.guvern.minister.mmjs
    graph.nodes.puteri.executiv.guvern.minister.mt
    graph.nodes.puteri.executiv.guvern.minister.mcsi
    graph.nodes.puteri.executiv.guvern.minister.map

    graph.edge.ministrulAparariiNationale.mapn.conduce()
    graph.edge.ministrulAfacerilorInterne.mai.conduce()
    graph.edge.ministrulAfacerilorExterne.mae.conduce()
    graph.edge.ministrulJustitiei.mj.conduce()
    graph.edge.ministrulEconomiei.me.conduce()
    graph.edge.ministrulFinantelorPublice.mfp.conduce()


    graph.nodes.csat(label = "CSAT\nConsiliul Suprem de Apărare a Ţării",
      tooltip =
        """Preşedintele României îndeplineşte funcţia de preşedinte al Consiliului Suprem de Apărare a Ţării.
          |Primul-ministru al Guvernului României îndeplineşte funcţia de vicepreşedinte al Consiliului Suprem de Apărare a Ţării.
          |Membrii Consiliului Suprem de Apărare a Ţării sunt: ministrul apărării naţionale, ministrul afacerilor interne, ministrul afacerilor externe, ministrul justiţiei, ministrul economiei, ministrul finanţelor publice, directorul Serviciului Român de Informaţii, directorul Serviciului de Informaţii Externe, şeful Statului Major al Apărării şi consilierul prezidențial pentru securitate naţională.
          |Secretarul Consiliului Suprem de Apărare a Ţării este numit de Preşedintele României şi are rang de consilier de stat în cadrul Administraţiei Prezidenţiale.
          |(Articolul 5 din Legea privind organizarea şi funcţionarea Consiliului Suprem de Apărare a Ţării)""".stripMargin)

    graph.edge.csat.presedinte.presedinte()
    graph.edge.csat.primMinistru.vicepresedinte()
    graph.edge.csat.ministrulAparariiNationale.membru()
    graph.edge.csat.ministrulAfacerilorInterne.membru()
    graph.edge.csat.ministrulAfacerilorExterne.membru()
    graph.edge.csat.ministrulJustitiei.membru()
    graph.edge.csat.ministrulEconomiei.membru()
    graph.edge.csat.ministrulFinantelorPublice.membru()
    graph.edge.csat.directorulSIE.membru()
    graph.edge.csat.directorulSRI.membru()
    graph.edge.csat.sefStatMajorAparare.membru()
    graph.edge.csat.consilierPrezidentialSecuritateNationala.membru()

    graph.nodes.agentiiInformatii(en="intelligence services",label="agenţiile de informații")
    graph.nodes.agentiiInformatii.SRI
    graph.nodes.agentiiInformatii.SIE
    graph.nodes.agentiiInformatii.STS
    graph.nodes.agentiiInformatii.DGIA

    graph.nodes.SRI(label="SRI\nServiciului Român de Informaţii")
    graph.nodes.SIE(label="SIE\nServiciului de Informaţii Externe")
    graph.nodes.STS(label="STS\nServiciul de Telecomunicații Speciale")
    graph.nodes.SPP(label="SPP\nServiciul de Protecţie şi Pază")
    graph.nodes.ministrulAparariiNationale.DGIA(label="DGIA\nDirecția Generală de Informații a Apărării",URL="https://en.wikipedia.org/wiki/General_Directorate_for_Defense_Intelligence")
    graph.nodes.DGIA.DIM(label="DIM\nDirecția Informații Militare",abelEn="Directorate for Military Intelligence - foreign intelligence")
    graph.nodes.DGIA.DSM(label="DSM\nDirecţia Siguranță Militară",labelEn="Directorate for Military Security - counter-intelligence")
    graph.nodes.MonitorulOficial
    graph.nodes.AdministratiaPrezidentiala
    graph.nodes.NATO
    graph.nodes.UE

    graph.nodes.SMA(label="Statului Major al Apărării")
    graph.nodes.consilierPrezidentialSecuritateNationala(label="consilierul prezidențial pentru securitate naţională")


    graph.nodes.puteri.presa
    graph.nodes.puteri.cetateni
    graph.edge.cetateni.asociatii.asociaza()

    graph.nodes.puteri.judiciar(label = "judiciar (aplică și interpretează legile)")
    graph.nodes.judiciar.judecatori
    graph.nodes.magistrati.judecatori
    graph.nodes.judecatori.ccr(label = "CCR\nCurtea Constitutionala a Romaniei\n9 judecatori\n9 ani", mandat = "9 ani")
    graph.nodes.judecatori.iccj(label = "ÎCCJ\nÎnalta Curte de Casație și Justiție.")

    graph.nodes.magistrati.procurori
    graph.nodes.procurori.dna(label = "DNA\nDepartamentul National Anticoruptie\n(fost PNA - Parchetul National Anticoruptie)")
    graph.nodes.procurori.piccj(label = "PICCJ\nPICCJ - Parchetul de pe langa Înalta Curte de Casație și Justiție.)")

    graph.nodes.asociatii.partide
    graph.nodes.asociatii.sindicate
    graph.nodes.asociatii.patronate
    graph.nodes.asociatii.asociatiiProfesionale(label = "Asociatii Profesionale")
    graph.nodes.asociatii.ong(label="ONG\nOrganizatii Neguvernamentale")

    graph.edge.csm.magistrati.propune()
    graph.edge.cedo.iccj.control(label = "corectie decizii")

    graph.edge.piccj.iccj.deserveste()


    graph.edge.constitutie.legi.controleaza()
    graph.edge.legi.regulamente.controleaza()

    graph.nodes.by(label = "by raisercostin & alexugoku (c) 2018")
    println(graph.toDot)
  }
}
