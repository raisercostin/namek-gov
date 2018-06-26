package org.raisercostin.namek.graph

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import org.raisercostin.jedi.Locations
import org.raisercostin.namek.gov.GovApp
import org.raisercostin.namek.gov.GovApp.Graph
import org.raisercostin.utils.ObjectUtils

import scala.util.Try

class NamekGraphTest {
  @Test def testAddNodeSubgraph() = {
    val graph = new Graph()
    graph.nodes.n1.n2
    graph.edge.n2.n3


    assertEquals(2,graph.n.size)
    assertEquals(1,graph.e.size)
    assertEquals(toPrettyString("""<graph id="G" edgedefault="undirected">
                   |    <node id="n1"><data key="d0">
                   |        <y:ProxyAutoBoundsNode>
                   |          <y:Realizers active="0">
                   |            <y:GroupNode>
                   |              <y:NodeLabel modelName="internal" modelPosition="t">n1</y:NodeLabel>
                   |              <y:State closed="false"/>
                   |            </y:GroupNode>
                   |            <y:GroupNode>
                   |              <y:NodeLabel modelName="internal" modelPosition="t">n1</y:NodeLabel>
                   |              <y:State closed="true"/>
                   |            </y:GroupNode>
                   |          </y:Realizers>
                   |        </y:ProxyAutoBoundsNode></data>  <graph id="n1:" edgedefault="undirected">
                   |    <node id="n2"><data key="d0"><y:ShapeNode><y:NodeLabel>n2</y:NodeLabel></y:ShapeNode></data></node>
                   |
                   |  </graph>
                   |</node>
                   |    <node id="n3"><data key="d0"><y:ShapeNode><y:NodeLabel>n3</y:NodeLabel></y:ShapeNode></data></node>
                   |    <edge source="n2" target="n3"/>
                   |  </graph>""".stripMargin),toPrettyString(graph.toGraphml))
  }
  @Test def testBug1() = {
    val graph = new Graph()
    graph.nodes.n1.n2
    graph.nodes.n1.n2.n3
    graph.nodes.n1.n2.n4
    graph.edge.n1.n3
    assertEquals(1, graph.e.size)
    assertEquals(toPrettyString("""<graph id="G" edgedefault="undirected">
                                  |    <node id="n1"><data key="d0">
                                  |        <y:ProxyAutoBoundsNode>
                                  |          <y:Realizers active="0">
                                  |            <y:GroupNode>
                                  |              <y:NodeLabel modelName="internal" modelPosition="t">n1</y:NodeLabel>
                                  |              <y:State closed="false"/>
                                  |            </y:GroupNode>
                                  |            <y:GroupNode>
                                  |              <y:NodeLabel modelName="internal" modelPosition="t">n1</y:NodeLabel>
                                  |              <y:State closed="true"/>
                                  |            </y:GroupNode>
                                  |          </y:Realizers>
                                  |        </y:ProxyAutoBoundsNode></data>  <graph id="n1:" edgedefault="undirected">
                                  |    <node id="n2"><data key="d0"><y:ShapeNode><y:NodeLabel>n2</y:NodeLabel></y:ShapeNode></data></node>
                                  |
                                  |  </graph>
                                  |</node>
                                  |    <node id="n3"><data key="d0"><y:ShapeNode><y:NodeLabel>n3</y:NodeLabel></y:ShapeNode></data></node>
                                  |    <edge source="n2" target="n3"/>
                                  |  </graph>""".stripMargin),toPrettyString(graph.toGraphml))
  }


  @Test def test1() = {
    Locations.file("target\\graph.graphml").writeContent(toPrettyString(GovApp.graphNodes.toFullGraphml).get)
    assertEquals(toPrettyString(Locations.classpath("sample1-expected.graphml").readContent),toPrettyString(GovApp.graphNodes.toFullGraphml))
  }

  def toPrettyString(xml: String, indent: Int=2): Try[String] = Try { // Turn xml string into a document
    import javax.xml.xpath.XPathFactory
    import org.w3c.dom.NodeList
    import javax.xml.parsers.DocumentBuilderFactory
    import javax.xml.transform.OutputKeys
    import javax.xml.transform.TransformerFactory
    import javax.xml.transform.dom.DOMSource
    import javax.xml.transform.stream.StreamResult
    import javax.xml.xpath.XPathConstants
    import java.io.ByteArrayInputStream
    import java.io.StringWriter

    val document = DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(new ByteArrayInputStream(xml.getBytes("utf-8")))
    // Remove whitespaces outside tags
    document.normalize()
    val xPath = XPathFactory.newInstance.newXPath
    val nodeList = xPath.evaluate("//text()[normalize-space()='']", document, XPathConstants.NODESET).asInstanceOf[NodeList]
    var i = 0
    while ( {
      i < nodeList.getLength
    }) {
      val node = nodeList.item(i)
      node.getParentNode.removeChild(node)

      {
        i += 1;
        i
      }
    }
    // Setup pretty print options
    val transformerFactory = TransformerFactory.newInstance
    transformerFactory.setAttribute("indent-number", indent)
    val transformer = transformerFactory.newTransformer
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    // Return pretty print xml string
    val stringWriter = new StringWriter
    transformer.transform(new DOMSource(document), new StreamResult(stringWriter))
    stringWriter.toString
  }
}
