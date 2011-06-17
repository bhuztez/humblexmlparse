humblexmlparse, a parser parsing XML into python objects, based on 
[simplexmlparse by Evan Jones](http://evanjones.ca/software/simplexmlparse.html)

Usage:

```python
>>> template = TemplateParser("""
... <root xmlns:parse="http://xml.par.se">
...   <item name="required" parse:count="1"/>
... </root>""", 'http://xml.par.se')
>>> root = parse_xml(
...     template,
...     """<root><item name='Hello, world!'/></root>""")
>>> print root.item["name"]
Hello, world!
```
