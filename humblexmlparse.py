#!/usr/bin/env python

#   humblexmlparse.py - a variant of simplexmlparse
#                       <http://evanjones.ca/software/simplexmlparse.html>
#   Copyright (C) 2010 bhuztez <bhuztez@gmail.com>
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU Affero General Public License as
#   published by the Free Software Foundation, either version 3 of the
#   License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU Affero General Public License for more details.
#
#   You should have received a copy of the GNU Affero General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

import xml.parsers.expat

# try:
#     from enum import Enum
#     Count = Enum('One', 'Optional', 'Positive', 'Any')
# except ImportError:
class Count:
    class One:      pass
    class Optional: pass
    class Any:      pass
    class Positive: pass


str2count = {
    '1': Count.One,
    '?': Count.Optional,
    '+': Count.Positive,
    '*': Count.Any}

attr2count = {
    'required': Count.One,
    'optional': Count.Optional}



ERROR_CODES = {
    'INVALID_COUNT':
        """Element '%s' has an invalid parse count attribute value: '%s' (must be '1', '?', '+', or '*')""",
    'INVALID_ATTRIBUTES':
        "Element '%s' is not permitted to have attributes %s",
    'INVALID_CHILD':
        "Element '%s' is not permitted to have child element '%s'",
    'MULTIPLE_CHILD':
        "Element '%s' cannot contain multiple '%s' elements",
    'INVALID_ROOT':
        "Invalid root tag '%s' (must be '%s')",
    'MISSING_ATTRIBUTES':
        "Element '%s' is missing required attributes %s",
    'MISSING_CHILDREN':
        "Element '%s' is missing required child elements %s",
    'CONTAINS_TEXT':
        "Element '%s' cannot contain non-whitespace text",

}

class XmlParseError(Exception):

    def __init__(self, line, column):
        self.line = line
        self.column = column

    def __str__(self):
        return "XmlParseError: %s: line %d, column %d"%(
            self.error, self.line, self.column)

class InvalidCountAttributeError(XmlParseError):
    def __init__(self, name, value, line, column):
        XmlParseError.__init__(self, line, column)
        self.error = ERROR_CODES['INVALID_COUNT']%(name, value)

class InvalidAttributesError(XmlParseError):
    def __init__(self, name, attributes, line, column):
        XmlParseError.__init__(self, line, column)
        attributes = ','.join(["'"+attr+"'" for attr in attributes])
        self.error = ERROR_CODES['INVALID_ATTRIBUTES']%(name, attributes)

class InvalidChildError(XmlParseError):
    def __init__(self, name, child, line, column):
        XmlParseError.__init__(self, line, column)
        self.error = ERROR_CODES['INVALID_CHILD']%(name, child)

class MultipleChildError(XmlParseError):
    def __init__(self, name, child, line, column):
        XmlParseError.__init__(self, line, column)
        self.error = ERROR_CODES['INVALID_CHILD']%(name, child)

class InvalidRootTagError(XmlParseError):
    def __init__(self, name, value, line, column):
         XmlParseError.__init__(self, line, column)
         self.error = ERROR_CODES['INVALID_ROOT']%(value, name)

class MissingAttributesError(XmlParseError):
    def __init__(self, name, attributes, line, column):
        XmlParseError.__init__(self, line, column)
        attributes = ','.join(["'"+attr+"'" for attr in attributes])
        self.error = ERROR_CODES['MISSING_ATTRIBUTES']%(name, attributes)

class MissingChildrenError(XmlParseError):
    def __init__(self, name, children, line, column):
        XmlParseError.__init__(self, line, column)
        children = ','.join(["'"+child+"'" for child in children])
        self.error = ERROR_CODES['MISSING_CHILDREN']%(name, children)

class ContainsTextError(XmlParseError):
    def __init__(self, name, line, column):
        XmlParseError.__init__(self, line, column)
        self.error = ERROR_CODES['CONTAINS_TEXT']%(name)


class TemplateNode:

    def __init__(self, name, count):
        self.name = name
        self.count = count
        self.contains_text = False

        self.attributes = {}
        self.children = {}

    def add_attribute(self, name, count):
        self.attributes[name] = count

    def add_child(self, childnode):
        name = childnode.name
        self.children[name] = childnode


class ExpatWrapper:
    def __init__(self):
        self.parser = xml.parsers.expat.ParserCreate(namespace_separator=' ')
        self.parser.StartElementHandler = self.start_element_handler
        self.parser.EndElementHandler = self.end_element_handler
        self.parser.CharacterDataHandler = self.character_data_handler


class TemplateParser(ExpatWrapper):

    def __init__(self, template_string, parser_namespace):
        ExpatWrapper.__init__(self)

        self.parser_namespace = parser_namespace
        self.node_stack = []
        self.root = None

        self.parser.Parse(template_string, True)


    def start_element_handler(self, name, attributes):
        try:
            count = str2count[
                attributes.get('%s count'%(self.parser_namespace), '?')]
        except KeyError:
            raise InvalidCountAttributeError(
                name,
                attributes['%s count'%(self.parser_namespace)],
                self.parser.CurrentLineNumber,
                self.parser.CurrentColumnNumber)

        node = TemplateNode(name, count)

        for name in attributes:
            if not name.startswith(self.parser_namespace):
                count = attr2count.get(attributes[name], Count.Optional)
                node.add_attribute(name, count)

        self.node_stack.append(node)


    def end_element_handler(self, name):
        node = self.node_stack.pop()
        try:
            self.node_stack[-1].add_child(node)
        except IndexError:
            self.root = node


    def character_data_handler(self, data):
        if not data.isspace():
            self.node_stack[-1].contains_text = True


class ElementNode:

    def __init__(self):
        self._attributes = {}
        self._text = ""

    def __getitem__(self, key):
        return self._attributes[key]

    def __setitem__(self, key, value):
        self._attributes[key] = value

    def __delitem__(self, key):
        del self._attributes[key]

    def __contains__(self, key):
        return key in self._attributes

    def __iter__(self):
        return iter(self._attributes)



class SimpleXmlParser(ExpatWrapper):

    def __init__(self, template, strict=True):
        ExpatWrapper.__init__(self)

        self.template_root = template.root
        self.strict = strict


    def parse(self, data):
        self.node_stack = []
        self.template_node_stack = []

        self.root = None
        self.skip = False

        self.parser.Parse(data, True)
        return self.root


    def start_element_handler(self, name, attributes):
        if self.skip:
            self.node_stack.append(None)
            return

        if not self.node_stack:
            if name != self.template_root.name:
                raise InvalidRootTagError(
                    self.template_root.name,
                    name,
                    self.parser.CurrentLineNumber,
                    self.parser.CurrentColumnNumber)

            template_node = self.template_root
        else:
            try:
                template_node = self.template_node_stack[-1].children[name]
            except KeyError:
                if self.strict:
                    raise InvalidChildError(
                        self.template_node_stack[-1].name,
                        name,
                        self.parser.CurrentLineNumber,
                        self.parser.CurrentColumnNumber)
                else:
                    self.skip = True
                    self.node_stack.append(None)
                    return

            if hasattr(self.node_stack[-1], name):
                raise MultipleChildError(
                    self.template_node_stack[-1].name,
                    name,
                    self.parser.CurrentLineNumber,
                    self.parser.CurrentColumnNumber)

        self.template_node_stack.append(template_node)

        node = ElementNode()

        invalid_attributes = [ name_
            for name_ in attributes
                if name_ not in template_node.attributes]
        if self.strict and any(invalid_attributes):
            raise InvalidAttributesError(
                    template_node.name,
                    invalid_attributes,
                    self.parser.CurrentLineNumber,
                    self.parser.CurrentColumnNumber)

        missing_attributes = [ name_
            for name_ in template_node.attributes
                if template_node.attributes[name_] is Count.One and
                   name_ not in attributes]
        if any(missing_attributes):
            raise MissingAttributesError(
                    template_node.name,
                    missing_attributes,
                    self.parser.CurrentLineNumber,
                    self.parser.CurrentColumnNumber)
        
        # XXX
        for name_ in template_node.attributes:
            if name_ not in attributes:
                attributes[name_] = None

        node._attributes.update(attributes)

        self.node_stack.append(node)


    def end_element_handler(self, name):
        if self.skip:
            self.node_stack.pop()

            if self.node_stack[-1]:
                self.skip = False
            return

        template_node = self.template_node_stack.pop()
        node = self.node_stack.pop()

        missing_children = [ name_
            for name_ in template_node.children
                if template_node.children[name_].count
                     in [Count.One, Count.Positive] and
                   name_ not in node.__dict__]
        if any(missing_children):
            raise MissingChildrenError(
                    template_node.name,
                    missing_children,
                    self.parser.CurrentLineNumber,
                    self.parser.CurrentColumnNumber)
        
        # XXX
        for name_ in template_node.children:
            if name_ not in node.__dict__:
                if template_node.children[name_].count is Count.Optional:
                    setattr(node, name_, None)
                elif template_node.children[name_].count is Count.Any:
                    setattr(node, name_, [])

        try:
            if template_node.count in [Count.One, Count.Optional]:
                setattr(self.node_stack[-1], name, node)
            else:
                nodes = getattr(self.node_stack[-1], name, [])
                nodes.append(node)
                setattr(self.node_stack[-1], name, nodes)

        except IndexError:
            self.root = node


    def character_data_handler(self, data):
        if self.skip:
            return
        # data is never ""

        if self.template_node_stack[-1].contains_text:
            self.node_stack[-1]._text += data
        elif not data.isspace():
            if self.strict:
                raise ContainsTextError(
                    self.template_node_stack[-1].name,
                    self.parser.CurrentLineNumber,
                    self.parser.CurrentColumnNumber)


def parse_xml(template, data, strict=True):
    parser = SimpleXmlParser(template, strict)
    return parser.parse(data)


def suite():
    import unittest

    class testTemplate(unittest.TestCase):
        def testInvalidCountAttributeError(self):
            self.assertRaises(InvalidCountAttributeError, TemplateParser,
"""<root xmlns:parse="http://xml.par.se">
  <item parse:count="0"/>
</root>""", 'http://xml.par.se')


    class testParse(unittest.TestCase):
        def setUp(self):
            self.template = TemplateParser("""
<root xmlns:parse="http://xml.par.se">
  <stat name="">a</stat>
  <item parse:count="*"/>
</root>
""", 'http://xml.par.se')

        def testAttribute(self):
            root = parse_xml(self.template, """<root><stat name="a"/></root>""")
            self.assertEqual(root.stat['name'], "a")

        def testText(self):
            root = parse_xml(self.template, """<root><stat>a</stat></root>""")
            self.assertEqual(root.stat._text, "a")

        def testList(self):
            root = parse_xml(self.template, """<root><item/></root>""")
            self.assertTrue(isinstance(root.item, list))


    class testError(unittest.TestCase):
        def setUp(self):
            self.template = TemplateParser("""
<root xmlns:parse="http://xml.par.se">
  <item name="required" parse:count="1"/>
</root>
""", 'http://xml.par.se')

        def testInvalidRootTagError(self):
            self.assertRaises(InvalidRootTagError,
                parse_xml, self.template, """<doc/>""")

        def testMissingAttributesError(self):
             self.assertRaises(MissingAttributesError,
                parse_xml, self.template, """<root><item/></root>""", False)

        def testMissingChildrenError(self):
             self.assertRaises(MissingChildrenError,
                parse_xml, self.template, """<root/>""", False)
        
        def testMultipleChildError(self):
             self.assertRaises(MultipleChildError,
                parse_xml, self.template, """<root><item name='a'/><item name='a'/></root>""", False)


    class testStrict(unittest.TestCase):
        def setUp(self):
            self.template = TemplateParser("""<root/>""", 'http://xml.par.se')

        def testInvalidAttributesError(self):
            self.assertRaises(InvalidAttributesError,
                parse_xml, self.template, """<root name="a"/>""")

        def testInvalidChildError(self):
            self.assertRaises(InvalidChildError,
                parse_xml, self.template, """<root><a/></root>""")

        def testContainsTextError(self):
            self.assertRaises(ContainsTextError,
                parse_xml, self.template, """<root>a</root>""")

    class testTolerant(unittest.TestCase):
        def setUp(self):
            self.template = TemplateParser("""<root/>""", 'http://xml.par.se')

        def testInvalidAttributes(self):
            parse_xml(self.template, """<root name="a"/>""", False)

        def testInvalidChild(self):
            parse_xml(self.template, """<root><a/></root>""", False)

        def testContainsText(self):
            parse_xml(self.template, """<root>a</root>""", False)


    return unittest.TestSuite([
        unittest.TestLoader().loadTestsFromTestCase(testTemplate),
        unittest.TestLoader().loadTestsFromTestCase(testParse),
        unittest.TestLoader().loadTestsFromTestCase(testError),
        unittest.TestLoader().loadTestsFromTestCase(testStrict),
        unittest.TestLoader().loadTestsFromTestCase(testTolerant),
    ])


if __name__ == "__main__":
    import unittest
    unittest.TextTestRunner(verbosity=2).run(suite())


