package org.raisercostin.namek.graph

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import org.raisercostin.jedi.Locations
import org.raisercostin.namek.gov.GovApp
import org.raisercostin.utils.ObjectUtils

import scala.util.Try

class NamekGraphTest {
  @Test def test1() = {
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
