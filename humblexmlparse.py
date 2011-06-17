#!/usr/bin/env python2

#   humblexmlparse.py - a variant of simplexmlparse
#                       <http://evanjones.ca/software/simplexmlparse.html>
#   Copyright (C) 2010,2011 bhuztez <bhuztez@gmail.com>
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

'''
A parser parsing XML into python objects, based on simplexmlparse by Evan 
Jones <http://evanjones.ca/software/simplexmlparse.html>

Usage:
    >>> template = TemplateParser("""
    ... <root xmlns:parse="http://xml.par.se">
    ...   <item name="required" parse:count="1"/>
    ... </root>""", 'http://xml.par.se')
    >>> root = parse_xml(
    ...     template,
    ...     """<root><item name='Hello, world!'/></root>""")
    >>> print root.item["name"]
    Hello, world!
'''

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

    ALL = {'1':One, '?': Optional, '+': Positive, '*': Any}

    @classmethod
    def is_single(cls, template_node):
        return template_node._count in [cls.One, cls.Optional]

    @classmethod
    def is_required(cls, template_node):
        return template_node._count in [cls.One, cls.Positive]

    @classmethod
    def ofAttribute(cls, count):
        return cls.One if count == 'required' else cls.Optional

    @classmethod
    def ofNode(cls, attrs, namespace):
        return cls.ALL[attrs.get('%s count'%(namespace), '?')]


class XmlParseError(Exception):

    def __init__(self, parser, *args):
        self.line = parser.parser.CurrentLineNumber
        self.column = parser.parser.CurrentColumnNumber

        if hasattr(self, 'format_args'):
            args = self.format_args(parser, *args)

        self.error = self.ERROR%args


    def __str__(self):
        return "XmlParseError: %s: line %d, column %d"%(
            self.error, self.line, self.column)


class ConcatMixin:
    def format_args(self, parser, name, elems):
        return (name, ','.join(["'"+e+"'" for e in elems]))


class InvalidCountAttributeError(XmlParseError):

    ERROR = """Element '%s' has an invalid parse count attribute value: '%s' (must be '1', '?', '+', or '*')"""

    def format_args(self, parser, name, attrs):
        return (name, attrs['%s count'%(parser.parser_namespace)])

class RedefineChildError(XmlParseError):
    ERROR = """Element '%s' has already been defined"""

class InvalidAttributesError(XmlParseError, ConcatMixin):
    ERROR = "Element '%s' is not permitted to have attributes %s"

class InvalidChildError(XmlParseError):
    ERROR = "Element '%s' is not permitted to have child element '%s'"

class DuplicateChildError(XmlParseError):
    ERROR = "Element '%s' cannot contain more than one '%s' elements"

class InvalidRootTagError(XmlParseError):
    ERROR = "Invalid root tag '%s' (must be '%s')"

class MissingAttributesError(XmlParseError, ConcatMixin):
    ERROR = "Element '%s' is missing required attributes %s"

class MissingChildrenError(XmlParseError, ConcatMixin):
    ERROR = "Element '%s' is missing required child elements %s"

class ContainsTextError(XmlParseError):
    ERROR = "Element '%s' cannot contain non-whitespace text"


class AttributeDict:
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


class ElementNode(AttributeDict):

    def __init__(self, initial):
        self._attributes = dict(initial)
        self._text = ""

    def _put_node(self, name, template_node, node):
        if Count.is_single(template_node):
            setattr(self, name, node)
        else:
            setattr(self, name, getattr(self, name, []) + [node])


class TemplateNode(AttributeDict):

    def __init__(self, name, count, initial):
        self._name = name
        self._count = count
        self._contains_text = False
        self._attributes = dict(initial)

    def _check_attrs(self, parser, attrs):
        missing_attrs = [ 
            attr for attr,count in self._attributes.items()
                if count is Count.One and attr not in attrs ]

        if any(missing_attrs):
            raise MissingAttributesError(parser, self._name, missing_attrs)

        invalid_attrs = [ attr for attr in attrs if attr not in self ]
        if parser.strict and any(invalid_attrs):
            raise InvalidAttributesError(parser, self._name, invalid_attrs)

        return [ (attr, attrs.get(attr, None)) 
                   for attr in self 
                       if attr not in invalid_attrs ]

    def _make_node(self, parser, attrs): 
        return ElementNode(self._check_attrs(parser, attrs))

    def _children(self, is_required):
        for name, template_node in self.__dict__.items():
            if not name.startswith('_'):
                if Count.is_required(template_node) == is_required:
                    yield (name, template_node)

    def _check_children(self, parser, node):
        missing_children = [
            name for (name, template_node) in self._children(True)
                if not hasattr(node, name)]

        if any(missing_children):
            raise MissingChildrenError(parser, self._name, missing_children)

        node.__dict__.update(
            [ (name, None if Count.is_single(template_node) else []) 
                for (name, template_node) in self._children(False) 
                    if not hasattr(node, name) ])


class ExpatWrapper:

    def __init__(self):
        self.parser = xml.parsers.expat.ParserCreate(namespace_separator=' ')
        self.parser.StartElementHandler = self.start_element_handler
        self.parser.EndElementHandler = self.end_element_handler
        self.parser.CharacterDataHandler = self.character_data_handler

    def start_element_handler(self, name, attributes):
        self.handlers = self.handlers[0](name, attributes) or self.handlers

    def end_element_handler(self, name):
        self.handlers = self.handlers[1](name) or self.handlers

    def character_data_handler(self, data):
        self.handlers = self.handlers[2](data) or self.handlers

    def get_initial_handlers(self):
        raise NotImplementedError


class TemplateParser(ExpatWrapper):

    def __init__(self, template_string, parser_namespace):
        ExpatWrapper.__init__(self)

        self.parser_namespace = parser_namespace
        self.node_stack = []
        self.root = None

        self.handlers = self.get_initial_handlers()
        self.parser.Parse(template_string, True)


    def get_initial_handlers(self):
        return (self.on_start, self.on_end, self.on_data)


    def on_start(self, name, attrs):
        try:
            count = Count.ofNode(attrs, self.parser_namespace)
        except KeyError:
            raise InvalidCountAttributeError(self, name, attrs)

        self.node_stack.append(TemplateNode(
            name, count, 
            [ (name, Count.ofAttribute(attrs[name]) ) 
                for name in attrs
                    if not name.startswith(self.parser_namespace+' ')]))


    def on_end(self, name):
        node = self.node_stack.pop()
        try:
            parent = self.node_stack[-1]
            if hasattr(parent, name):
                raise RedefineChildError(self, name)
            setattr(parent, name, node)
        except IndexError:
            self.root = node
            return (None, None, None)


    def on_data(self, data):
        if not data.isspace():
            self.node_stack[-1]._contains_text = True



class HumbleXmlParser(ExpatWrapper):

    def __init__(self, template, strict=True):
        ExpatWrapper.__init__(self)

        self.template_root = template.root
        self.strict = strict


    def parse(self, data):
        self.node_stack = []
        self.template_node_stack = []
        self.root = None

        self.handlers = self.get_initial_handlers()
        self.parser.Parse(data, True)

        return self.root


    @property
    def top_node(self):
        return self.node_stack[-1]

    @property
    def top_template(self):
        return self.template_node_stack[-1]

    def get_initial_handlers(self):
        return (self.on_start_root_tag, None, None)


    def on_start_root_tag(self, name, attrs):
        if name != self.template_root._name:
            raise InvalidRootTagError(self, name, self.template_root._name)

        self.template_node_stack.append(self.template_root)
        self.node_stack.append(self.template_root._make_node(self, attrs))

        return (self.on_start, self.on_end, self.on_data)


    def on_start(self, name, attrs):
        try:
            template_node = getattr(self.top_template, name)
        except AttributeError:
            if self.strict:
                raise InvalidChildError(self, self.top_template._name, name)

            self.node_stack.append(None)
            return (self.skip_start, self.skip_end, self.skip_data)

        if Count.is_single(template_node) and hasattr(self.top_node, name):
            raise DuplicateChildError(self, self.top_template._name, name)

        self.template_node_stack.append(template_node)
        self.node_stack.append(template_node._make_node(self, attrs))


    def on_end(self, name):
        node = self.node_stack.pop()
        template_node = self.template_node_stack.pop()

        template_node._check_children(self, node)
        try:
            self.top_node._put_node(name, template_node, node)
        except IndexError:
            self.root = node
            return (None, None, None)


    def on_data(self, data):
        if self.top_template._contains_text:
            self.top_node._text += data
        elif self.strict and not data.isspace():
            raise ContainsTextError(self, self.top_template._name)


    def skip_start(self, name, attrs):
        self.node_stack.append(name)

    def skip_end(self, name):
        if self.node_stack.pop() is None:
            return (self.on_start, self.on_end, self.on_data)

    def skip_data(self, data):
        pass


def parse_xml(template, data, strict=True):
    parser = HumbleXmlParser(template, strict)
    return parser.parse(data)


def suite():
    import unittest, doctest

    class testTemplate(unittest.TestCase):
        def testSuccess(self):
            node = TemplateParser("""
<root xmlns:parse="http://xml.par.se">
    <item attr="required" />
</root>
""", 'http://xml.par.se')
            self.assertEqual(node.root.item['attr'], Count.One)

        def testRedefineChildError(self):
            self.assertRaises(RedefineChildError, TemplateParser, 
"""<root xmlns:parse="http://xml.par.se">
  <item/><item/>
</root>""", 'http://xml.par.se')


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
            root = parse_xml(self.template, """<root></root>""")
            self.assertTrue(isinstance(root.item, list))
            self.assertEqual(len(root.item), 0)
            root = parse_xml(self.template, """<root><item/></root>""")
            self.assertEqual(len(root.item), 1)
            root = parse_xml(self.template, """<root><item/><item/></root>""")
            self.assertEqual(len(root.item), 2)


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
        
        def testDuplicateChildError(self):
             self.assertRaises(DuplicateChildError,
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
            root = parse_xml(self.template, """<root name="a"/>""", False)
            self.assertFalse("name" in root)

        def testInvalidChild(self):
            root = parse_xml(self.template, """<root><a><b/></a></root>""", False)
            self.assertFalse(hasattr(root, 'a'))

        def testContainsText(self):
            root = parse_xml(self.template, """<root>a</root>""", False)
            self.assertEqual(root._text, "")

    return unittest.TestSuite([
        unittest.TestLoader().loadTestsFromTestCase(testTemplate),
        unittest.TestLoader().loadTestsFromTestCase(testParse),
        unittest.TestLoader().loadTestsFromTestCase(testError),
        unittest.TestLoader().loadTestsFromTestCase(testStrict),
        unittest.TestLoader().loadTestsFromTestCase(testTolerant),
        doctest.DocTestSuite(),
    ])


if __name__ == "__main__":
    import unittest
    unittest.TextTestRunner(verbosity=2).run(suite())

