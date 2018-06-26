package org.raisercostin.namek.gov

import scala.util.Try

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

    def toCypher: String = if (attributes.isEmpty) "" else s""" {${attributes.map(x => escape(x._1) + "=" + escape(x._2)).mkString(",")}}"""

    def toGraphml: String = ""

    def escapeGraphml(text: String): String = escape2(text, new StringBuilder).toString

    val pairs = Map(
      "lt" -> '<',
      "gt" -> '>',
      "amp" -> '&',
      "quot" -> '"',
      "apos" -> '\''
    )
    val escMap = (pairs - "apos") map { case (s, c) => c -> ("&%s;" format s) }

    final def escape2(text: String, s: StringBuilder): StringBuilder = {
      // Implemented per XML spec:
      // http://www.w3.org/International/questions/qa-controls
      text.iterator.foldLeft(s) { (s, c) =>
        escMap.get(c) match {
          case Some(str) => s ++= str
          case _ if c >= ' ' || "\n\r\t".contains(c) => s += c
          case _ => s // noop
        }
      }
    }
  }

  case class Node(id: String, var subgraph: Option[Graph] = None) extends WithAttributes {
    def findNode(nodeId: String): Option[Node] =
      if (nodeId == id)
        Some(this)
      else
        subgraph.flatMap(_.findNodeInAllGraph(nodeId))

    def parentAddSubgraphNode(nodeId: String): Node = {
      if (subgraph.isEmpty)
        subgraph = Some(Graph(s"$id:"))
      subgraph.get.findOrCreateNode(nodeId)
    }

    override def toDot: String = s"$id${super.toDot}"

    override def toCypher: String = s"CREATE ($id:Actor ${super.toCypher})"

    override def toGraphml: String = {
      val nodeId = escapeGraphml(attr.getOrElse("label", id))
      val content = if (subgraph.isEmpty)
        s"""<y:ShapeNode><y:NodeLabel>${nodeId}</y:NodeLabel></y:ShapeNode>""".stripMargin
      else
        s"""
           |        <y:ProxyAutoBoundsNode>
           |          <y:Realizers active="0">
           |            <y:GroupNode>
           |              <y:NodeLabel modelName="internal" modelPosition="t">$nodeId</y:NodeLabel>
           |              <y:State closed="false"/>
           |            </y:GroupNode>
           |            <y:GroupNode>
           |              <y:NodeLabel modelName="internal" modelPosition="t">$nodeId</y:NodeLabel>
           |              <y:State closed="true"/>
           |            </y:GroupNode>
           |          </y:Realizers>
           |        </y:ProxyAutoBoundsNode>""".stripMargin
      s"""<node id="$id"><data key="key-node-label">""" + content + s"""</data>${subgraph.map(_.toGraphml).getOrElse("")}</node>"""
    }
  }


  case class Edge(src: Node, dst: Node) extends WithAttributes {
    override def attributes: Map[String, String] =
      if (attr.contains("label"))
        attr
      else
        attr.get("kind").map(x => attr + ("label" -> x)).getOrElse(attr)

    override def toDot: String = s"""${src.id}->${dst.id}${super.toDot}"""

    override def toCypher: String = s"""CREATE (${src.id})-[:$label${super.toCypher}]->(${dst.id})"""

    override def toGraphml: String =
      s"""<edge source="${src.id}" target="${dst.id}">
         |<data key="key-edge-label">
         |  <y:PolyLineEdge>
         |    <y:Arrows source="none" target="standard"/>
         |    <y:EdgeLabel modelName="six_pos" modelPosition="ttail" preferredPlacement="source">$label</y:EdgeLabel>
         |    <y:BendStyle smoothed="false"/>
         |  </y:PolyLineEdge>
         |</data></edge>""".stripMargin

    def label:String = attr.get("kind").getOrElse("")
  }

  import scala.language.dynamics

  case class Graph(val id: String = "G", var n: Set[Node] = Set(), var e: Seq[Edge] = Seq()) {
    val nodes = new NodeBuilder(this)
    val edge = new EdgeBuilder(this)

    def addEdge(src: String, dst: String): Edge =
      e.find(x => x.src.id == src && x.dst.id == dst).getOrElse {
        val edge = new Edge(findOrCreateNode(src), findOrCreateNode(dst))
        e = e :+ edge
        edge
      }

    def findOrCreateNode(nodeId: String): Node =
      findNodeInAllGraph(nodeId).getOrElse(addNode(nodeId))

    private def findNode(nodeId: String): Option[Node] =
      n.find(_.id == nodeId)

    def findNodeInAllGraph(nodeId: String): Option[Node] =
      n.toStream.flatMap(_.findNode(nodeId)).headOption

    def findOrCreateNodeWithParent(nodeId: String, parent: Node): Node = {
      //val node = findNode(nodeId)
      //parent.getOrCreateSubgraph(nodeId).findOrCreateNode(nodeId)
      val node = parent.parentAddSubgraphNode(nodeId)
      //addEdge(parent.id, nodeId).addAttribute("label", "include").addAttribute("weight", "2").addAttribute("kind", "include")
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

    def toCypher =
      s"""
         |CREATE CONSTRAINT ON (actor:Actor) ASSERT actor.title IS UNIQUE;
         |${n.map(_.toCypher).mkString("\n  ")}
         |${e.map(_.toCypher).mkString("\n  ")}
         |MATCH (n:Actor) OPTIONAL MATCH (n:Actor)-[r:include]->(sub) RETURN DISTINCT n,r,sub;
      """.stripMargin

    //    <graphml xmlns="http://graphml.graphdrawing.org/xmlns"
    //             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    //             xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns
    //     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">
    def toGraphml: String =
      s"""|  <graph id="$id" edgedefault="undirected">
          |    ${n.map(_.toGraphml).mkString("\n    ")}
          |    ${e.map(_.toGraphml).mkString("\n    ")}
          |  </graph>
          |""".stripMargin

    def toFullGraphml: String =
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<graphml
         | xmlns="http://graphml.graphdrawing.org/xmlns"
         | xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         | xmlns:y="http://www.yworks.com/xml/graphml"
         | xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.0/ygraphml.xsd">
         |  <key for="node" id="key-node-label" yfiles.type="nodegraphics"/>
         |  <key for="edge" id="key-edge-label" yfiles.type="edgegraphics"/>
         |  ${toGraphml}
         |</graphml>
         |""".stripMargin
  }

  /*
<graphml
 xmlns="http://graphml.graphdrawing.org/xmlns"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xmlns:y="http://www.yworks.com/xml/graphml"
 xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.0/ygraphml.xsd">

  <key for="node" id="key-node-label" yfiles.type="nodegraphics"/>

  <graph id="G" edgedefault="directed">
    <node id="a"><data key="key-node-label"><y:ShapeNode><y:NodeLabel>a</y:NodeLabel></y:ShapeNode></data></node>

  </graph>
</graphml>
    */
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
      val edge = graph.addEdge(src, dest);
      new EdgeBuilder3(graph, edge)
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

  def graphGov: Graph = {
    //Comisia Europeană e al puterii executive. Pe lângă astea, Consiliul Uniunii Europene are rol de o a doua cameră legislativă și Consiliul European are în principal roluri executive, cred. Mai mult, nu prea se ocupă niciuna strict cu un singur tip de putere.
    val graph = new Graph()
    graph.nodes.international.CEDO(label = "CEDO\nCurtea European a Drepturilor Omului")
    graph.nodes.international.UE(label = "UE\nUniunea Europeana")
    graph.nodes.international.NATO(label = "NATO\nNorth Atlantic Treaty Organization")
    graph.nodes.international.ONU(label = "ONU\nOrganizatia Natiunilor Unite")
    graph.nodes.international.CJUE(label = "CJUE \n Curtea de Justiție a Uniunii Europene")
    graph.nodes.international.europe(URL = "https://europa.eu/european-union/about-eu/institutions-bodies_en")
    graph.nodes.international.europe.comisiaEuropeana(label = "CE\nComisia Europeana \n propune si implementeaza legislatia, monitorizeaza tratate si functionarea zilnica a UE")
    graph.nodes.international.europe.consiliulEuropean(label = "Consiliul European \n directia strategica care decide directia politica generala a UE. Sefii de state si de guverne ai statelor UE")
    graph.nodes.international.europe.consiliulUniuniiEuropene(label = "Consiliul Uniunii Europene \n ministrii din guverne, care impart puterea bugetara si legislativa cu Parlamentul European")
    graph.nodes.international.europe.parlamentulEuropean(label = "EP\nParlamentul European \n propune si implementeaza legislatia", description = "Unicul organ EU ales. Reprezinta cei 500 de milioane de ctateni europeni.")
    graph.nodes.international.europe.presedinteleComisieiEuropene(label = "EP\nParlamentul European \n propune si implementeaza legislatia")

    graph.edge.parlamentulEuropean.presedinteleComisieiEuropene.alege()
    graph.edge.legislativ.parlamentulEuropean
    graph.edge.legislativ.consiliulUniuniiEuropene
    graph.edge.judiciar.CJUE
    graph.edge.executiv.comisiaEuropeana
    graph.edge.executiv.consiliulEuropean

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

    graph.nodes.puteri.executiv(tooltip = "(guvernează - implementarea legilor în practică și cu administrarea birocrației de stat)")
    graph.nodes.puteri.executiv.presedinte(URL = "http://www.contributors.ro/dezbatere/despre-o-inertie-politica-si-constitutionala-presedintele-romaniei-parte-a-puterii-executive/")
    graph.nodes.puteri.executiv.agentiiInformatii
    graph.nodes.puteri.executiv.guvern(tooltip = "Guvernul însă poate propune legi spre aprobare de către legislativ.")
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


    graph.nodes.CSAT(label = "CSAT\nConsiliul Suprem de Apărare a Ţării",
      tooltip =
        """Preşedintele României îndeplineşte funcţia de preşedinte al Consiliului Suprem de Apărare a Ţării.
          |Primul-ministru al Guvernului României îndeplineşte funcţia de vicepreşedinte al Consiliului Suprem de Apărare a Ţării.
          |Membrii Consiliului Suprem de Apărare a Ţării sunt: ministrul apărării naţionale, ministrul afacerilor interne, ministrul afacerilor externe, ministrul justiţiei, ministrul economiei, ministrul finanţelor publice, directorul Serviciului Român de Informaţii, directorul Serviciului de Informaţii Externe, şeful Statului Major al Apărării şi consilierul prezidențial pentru securitate naţională.
          |Secretarul Consiliului Suprem de Apărare a Ţării este numit de Preşedintele României şi are rang de consilier de stat în cadrul Administraţiei Prezidenţiale.
          |(Articolul 5 din Legea privind organizarea şi funcţionarea Consiliului Suprem de Apărare a Ţării)""".stripMargin)

    graph.edge.CSAT.presedinte.presedinte()
    graph.edge.CSAT.primMinistru.vicepresedinte()
    graph.edge.CSAT.ministrulAparariiNationale.membru()
    graph.edge.CSAT.ministrulAfacerilorInterne.membru()
    graph.edge.CSAT.ministrulAfacerilorExterne.membru()
    graph.edge.CSAT.ministrulJustitiei.membru()
    graph.edge.CSAT.ministrulEconomiei.membru()
    graph.edge.CSAT.ministrulFinantelorPublice.membru()
    graph.edge.CSAT.directorulSIE.membru()
    graph.edge.CSAT.directorulSRI.membru()
    graph.edge.CSAT.sefStatMajorAparare.membru()
    graph.edge.CSAT.consilierPrezidentialSecuritateNationala.membru()

    graph.nodes.agentiiInformatii(en = "intelligence services", label = "agenţiile de informații")
    graph.nodes.agentiiInformatii.SRI
    graph.nodes.agentiiInformatii.SIE
    graph.nodes.agentiiInformatii.STS
    graph.nodes.agentiiInformatii.SPP
    graph.nodes.agentiiInformatii.DGIA

    graph.nodes.SRI(label = "SRI\nServiciului Român de Informaţii")
    graph.nodes.SIE(label = "SIE\nServiciului de Informaţii Externe")
    graph.nodes.STS(label = "STS\nServiciul de Telecomunicații Speciale")
    graph.nodes.SPP(label = "SPP\nServiciul de Protecţie şi Pază")
    graph.nodes.ministrulAparariiNationale.DGIA(label = "DGIA\nDirecția Generală de Informații a Apărării", URL = "https://en.wikipedia.org/wiki/General_Directorate_for_Defense_Intelligence")
    graph.nodes.ministrulAparariiNationale.SMA(label = "Statului Major al Apărării")
    graph.nodes.DGIA.DIM(label = "DIM\nDirecția Informații Militare", abelEn = "Directorate for Military Intelligence - foreign intelligence")
    graph.nodes.DGIA.DSM(label = "DSM\nDirecţia Siguranță Militară", labelEn = "Directorate for Military Security - counter-intelligence")
    graph.nodes.MonitorulOficial
    graph.nodes.AdministratiaPrezidentiala

    graph.nodes.consilierPrezidentialSecuritateNationala(label = "consilierul prezidențial pentru securitate naţională")


    graph.nodes.puteri.presa
    graph.nodes.puteri.cetateni
    graph.edge.cetateni.asociatii.asociaza()

    graph.nodes.puteri.judiciar(label = "judiciar (aplică și interpretează legile)")
    graph.nodes.judiciar.judecatori
    graph.nodes.magistrati.judecatori
    graph.nodes.judecatori.CCR(label = "CCR\nCurtea Constitutionala a Romaniei\n9 judecatori\n9 ani", mandat = "9 ani")
    graph.nodes.judecatori.ICCJ(label = "ÎCCJ\nÎnalta Curte de Casație și Justiție \n (pana in 2003 - CSJ - Curtea Supremă de Justiție)", tooltip = "Instanța judiciară supremă în ierarhia instanțelor judecătorești din România")
    graph.nodes.judecatori.IJ(label = "IJ \n Inspectia Judiciara")
    graph.nodes.judecatori.CSM(label = "CSM \n Consiliul Superior al Magistraturii")
    graph.nodes.instanteJudecatoresti(label = "Instanţe judecătoreşti")
    graph.nodes.instanteJudecatoresti.ICCJ
    graph.nodes.instanteJudecatoresti.curtiApel(label = "Curti Apel (15)")
    graph.nodes.instanteJudecatoresti.tribunale(label = "Tribunale (42 - pe judete)")
    graph.nodes.instanteJudecatoresti.tribunaleSpecializate
    graph.nodes.instanteJudecatoresti.instanteMilitare
    graph.nodes.instanteJudecatoresti.judecatorii(label = "Judecatorii (188 - 11 nu functioneaza)")
    graph.nodes.curtiApel.tribunale
    graph.nodes.curtiApel.tribunaleSpecializate
    //Tribunalul Militar Teritorial Bucureşti şi Curtea Militară de Apel Bucureşti, Tribunalele militare funcţionează în municipiile Bucureşti, Cluj-Napoca şi Timişoara, iar Tribunalul Militar Teritorial şi Curtea Militară de Apel, în municipiul Bucureşti.
    graph.nodes.parchet.parchetCurtiApel
    graph.nodes.parchet.parchetTribunale
    graph.nodes.parchet.parchetJudecatorii
    graph.nodes.parchet.parchetMilitar
    graph.edge.parchetCurtiApel.curtiApel.deserveste()
    graph.edge.parchetTribunale.tribunale.deserveste()
    graph.edge.parchetJudecatorii.judecatorii.deserveste()
    graph.edge.parchetMilitar.instanteMilitare.deserveste()

    //others
    graph.nodes.avocatulPoporului
    graph.nodes.primari
    graph.nodes.prefecti
    graph.nodes.consilulJudetean
    graph.nodes.ANI
    graph.nodes.DIICOT(label = "DIICOT \n Direcția de Investigare a Infracțiunilor de Criminalitate Organizată și Terorism")

    graph.edge.ICCJ.SRI.protocol(URL = "http://www.ziare.com/stiri/csm/consiliul-superior-al-magistraturii-inspectia-judiciara-si-ICCJ-au-incheiat-protocoale-de-cooperare-cu-sri-1508549")
    graph.edge.IJ.SRI.protocol()
    graph.edge.PICCJ.SRI.protocol()
    graph.edge.CSM.SRI.protocol()

    graph.edge.deputati.CCR.numeste(label = "numeste\n3 judecatori")
    graph.edge.senatori.CCR.numeste(label = "numeste\n3 judecatori")
    graph.edge.presedinte.CCR.numeste(label = "numeste\n3 judecatori")

    graph.edge.presedinte.AdministratiaPrezidentiala.conduce()
    graph.edge.presedinte.CSAT.conduce()
    graph.edge.presedinte.primMinistru.desemneaza()
    graph.edge.presedinte.guvern.numeste()
    graph.edge.presedinte.parlament.dizolva()
    graph.edge.presedinte.referendum.initiere()

    graph.nodes.magistrati.procurori
    graph.nodes.parchet.DNA(label = "DNA\nDepartamentul National Anticoruptie\n(fost PNA - Parchetul National Anticoruptie)")
    graph.nodes.parchet.PICCJ(label = "PICCJ\nPICCJ - Parchetul de pe langa Înalta Curte de Casație și Justiție.)")
    graph.nodes.DNA.PSDNA(label = "Procurorul şef al DNA")
    graph.nodes.DNA.PG(label = "Procurorul General")
    graph.nodes.PICCJ.DNA

    graph.nodes.asociatii.partide(label = "partide politice")
    graph.nodes.asociatii.sindicate
    graph.nodes.asociatii.patronate
    graph.nodes.asociatii.asociatiiProfesionale(label = "Asociatii Profesionale")
    graph.nodes.asociatii.asociatiiProfesionale.unjr(label = "UNJR \n Uniunea Nationala a Judecatorilor din Romania")
    graph.nodes.asociatii.asociatiiProfesionale.amr(label = "AMR \n Asociatia Magistratilor din Romania")
    graph.nodes.asociatii.ong(label = "ONG \n Organizatii Neguvernamentale")

    graph.edge.magistrati.amr.asociati()
    graph.edge.judecatori.unjr.asociati()
    graph.edge.CSM.magistrati.propune()
    graph.edge.CEDO.ICCJ.control(label = "corectie decizii")

    graph.edge.PICCJ.ICCJ.deserveste()


    graph.edge.constitutie.legi.controleaza()
    graph.edge.legi.regulamente.controleaza()
    graph.edge.legi.MonitorulOficial.publica()

    graph
  }

  def graphNodes: Graph = {
    //Comisia Europeană e al puterii executive. Pe lângă astea, Consiliul Uniunii Europene are rol de o a doua cameră legislativă și Consiliul European are în principal roluri executive, cred. Mai mult, nu prea se ocupă niciuna strict cu un singur tip de putere.
    val graph = new Graph()
    graph.nodes.n1
    graph.nodes.g1.n2
    graph.nodes.g1.n3
    graph.nodes.g2.n4
    graph.nodes.g2.n5
    graph.nodes.g2.g3
    graph.nodes.g3.n5

    graph.edge.n1.g1
    graph.edge.n1.n2
    graph.edge.n1.n4
    graph.edge.n2.n3
    graph.edge.n2.g2
    graph.edge.n2.n3
    graph.edge.g1.g2
    graph
  }

  def printToFile(content: String, location: String = "C:/Users/jtdoe/Desktop/WorkSheet.txt"): Try[Unit] =
    Try(location).map { l => val f = new java.io.File(l); f.getParentFile.mkdirs(); f }.map(new java.io.PrintWriter(_)).map { f =>
      try {
        f.write(content)
      } finally {
        f.close
      }
    }

  def main(args: Array[String]): Unit = {
    println("//view at http://www.webgraphviz.com/")
    println("//various engine(dot) and formats(png-image-element) to save as png at https://dreampuf.github.io/GraphvizOnline/")
    //val graph = graphNodes
    val graph = graphGov
    graph.nodes.by(label = "by raisercostin & alexugoku (c) 2018")
    //println(graph.toDot)
    //println(graph.toCypher)
    println(graph.toFullGraphml)
    printToFile(graph.toFullGraphml, "target/graph-gov.graphml")
  }
}
