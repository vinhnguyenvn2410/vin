#I learned from a workshop at school about a tip to improve the effectiveness of your resume. By simply including some of the words
from the job descriptions, you can increase your chances. To find the most popular and relevant words, you can use the short pieces of code below. You would have to eliminate some of the meaningless info which i will try to find a solution on a later day. Gluck!
For the .txt file, I pasted a few job descriptions into it. You can do a few job descriptions at a time or more :D.

```python
import re
from collections import Counter, OrderedDict

with open('C:/Users/vinhn/Desktop/res.txt') as f:
    passage = f.read()

words = re.findall(r'\w+', passage)

cap_words = [word.upper() for word in words]

word_counts = Counter(cap_words)

```


```python
word_counts
```




    Counter({'3': 2,
             '5': 1,
             '7': 1,
             'A': 10,
             'ABILITY': 9,
             'ACADEMIC': 1,
             'ACCREDITED': 1,
             'ACHIEVE': 1,
             'ACQUIRE': 1,
             'ACROSS': 3,
             'ACTIONABLE': 2,
             'ACTIONS': 2,
             'ACTIVITIES': 2,
             'AD': 1,
             'ADVANCE': 1,
             'ADVANCED': 5,
             'ALSO': 1,
             'AMBIGUOUS': 2,
             'AN': 1,
             'ANALYSES': 1,
             'ANALYSIS': 7,
             'ANALYTICS': 5,
             'ANALYZE': 3,
             'AND': 58,
             'ARE': 2,
             'AREAS': 1,
             'ARRAY': 1,
             'ARTICULATE': 1,
             'AS': 6,
             'ASSESS': 1,
             'ATTENTION': 1,
             'ATTITUDE': 1,
             'AUDIENCES': 1,
             'AUDITOR': 1,
             'AWARENESS': 1,
             'AZURE': 1,
             'BA': 1,
             'BACHELOR': 1,
             'BANKING': 1,
             'BASED': 2,
             'BASIC': 1,
             'BE': 4,
             'BEING': 2,
             'BEST': 2,
             'BETTER': 1,
             'BI': 3,
             'BIG': 1,
             'BS': 1,
             'BUILD': 1,
             'BUILDING': 1,
             'BUSINESS': 13,
             'BY': 1,
             'C': 1,
             'CAN': 2,
             'CAPABILITIES': 1,
             'CERTIFICATION': 1,
             'CERTIFIED': 2,
             'CHANGING': 1,
             'CLEAN': 1,
             'CLEAR': 3,
             'CODE': 1,
             'COLLABORATE': 1,
             'COLLABORATION': 3,
             'COLLECTION': 1,
             'COMBINATION': 1,
             'COMMENTARY': 1,
             'COMMUNICATION': 2,
             'COMMUNICATIONS': 2,
             'COMPLEX': 2,
             'COMPLIANCE': 3,
             'COMPUTER': 2,
             'CONNECTIVITY': 1,
             'CONSULTATIVE': 1,
             'CONTACTS': 1,
             'CONTINUALLY': 1,
             'CORRECT': 1,
             'CREATE': 2,
             'CREATING': 1,
             'CREDENTIALS': 1,
             'CROSS': 1,
             'CURRENT': 1,
             'CUSTOMER': 1,
             'DASHBOARD': 1,
             'DASHBOARDS': 2,
             'DATA': 33,
             'DATABASES': 2,
             'DEEP': 1,
             'DEFINE': 4,
             'DEGREE': 2,
             'DELIVER': 3,
             'DEMONSTRATED': 1,
             'DEPTH': 1,
             'DESIGN': 2,
             'DETAIL': 1,
             'DETERMINE': 1,
             'DEVELOP': 5,
             'DEVELOPING': 1,
             'DEVELOPMENT': 2,
             'DIMENSIONALITY': 1,
             'DIRTY': 2,
             'DISCIPLINES': 1,
             'DIVES': 1,
             'DO': 1,
             'DOMAIN': 1,
             'DOMAINS': 1,
             'DRIVE': 1,
             'DRIVEN': 3,
             'DRIVERS': 1,
             'E': 1,
             'EACH': 1,
             'EASILY': 1,
             'ECONOMICS': 1,
             'EFFICIENCY': 1,
             'ENABLE': 1,
             'ENGINEERING': 5,
             'ENGINEERS': 1,
             'ENSURE': 3,
             'ENVIRONMENT': 3,
             'ESTABLISH': 1,
             'ETC': 2,
             'EVALUATION': 1,
             'EXCEL': 4,
             'EXCEPTIONAL': 3,
             'EXECUTIVE': 1,
             'EXPERIENCE': 5,
             'EXPERTISE': 2,
             'FAMILIARITY': 2,
             'FEDERAL': 1,
             'FILTER': 1,
             'FINANCE': 3,
             'FINANCIAL': 1,
             'FINDINGS': 1,
             'FOCUS': 1,
             'FOR': 5,
             'FORECASTS': 2,
             'FORMULAS': 1,
             'FROM': 4,
             'FUNCTIONS': 2,
             'FUNNELS': 1,
             'FUTURE': 1,
             'G': 1,
             'GAMING': 1,
             'GATHERED': 1,
             'GREAT': 1,
             'GROW': 1,
             'HADOOP': 1,
             'HANDS': 2,
             'HARD': 1,
             'HAVE': 1,
             'HEALTH': 2,
             'HIGH': 2,
             'HIVE': 1,
             'HOLISTIC': 1,
             'HOW': 1,
             'IDEAL': 1,
             'IDENTIFY': 1,
             'IMPACTS': 1,
             'IMPLEMENT': 1,
             'IMPROVE': 2,
             'IMPROVED': 1,
             'IMPROVEMENT': 1,
             'IN': 17,
             'INCLUDES': 2,
             'INCLUDING': 1,
             'INCOMPLETE': 2,
             'INDICATORS': 1,
             'INFLUENCE': 1,
             'INFORMATION': 3,
             'INGEST': 1,
             'INITIATIVES': 1,
             'INNOVATIVE': 1,
             'INSIGHTS': 4,
             'INTELLIGENCE': 1,
             'INTERACT': 1,
             'INTERNAL': 1,
             'INTERPRET': 2,
             'INTO': 2,
             'INVENTORIES': 1,
             'IS': 3,
             'KEEP': 1,
             'KEY': 2,
             'KNOWLEDGE': 3,
             'LAKE': 1,
             'LARGE': 3,
             'LAWS': 1,
             'LEAD': 3,
             'LEARNINGS': 1,
             'LOCATE': 2,
             'M365': 1,
             'MAINTAIN': 1,
             'MAKE': 1,
             'MANAGEMENT': 4,
             'MANGER': 1,
             'MANIPULATE': 1,
             'MANIPULATION': 2,
             'MANNER': 1,
             'MARKETING': 2,
             'MASTERS': 1,
             'MATH': 1,
             'MBA': 1,
             'MBRS': 1,
             'MEET': 1,
             'MEETINGS': 1,
             'MET': 1,
             'METICULOUS': 1,
             'METRIC': 1,
             'METRICS': 3,
             'MINIMUM': 2,
             'MINING': 3,
             'MONITORING': 1,
             'MS': 1,
             'MULTIPLE': 1,
             'MUST': 2,
             'NEEDS': 3,
             'NEW': 2,
             'NOT': 1,
             'OF': 18,
             'OFFICE': 2,
             'ON': 11,
             'ONGOING': 2,
             'OPERATIONALIZE': 1,
             'OPPORTUNITIES': 1,
             'OPTIMAL': 1,
             'OPTIMIZE': 1,
             'OR': 11,
             'ORAL': 1,
             'ORDER': 2,
             'ORGANIZATION': 3,
             'ORGANIZED': 1,
             'ORIENTATION': 1,
             'OTHER': 2,
             'OTHERS': 1,
             'OUR': 6,
             'OUTCOMES': 1,
             'OUTLOOKS': 1,
             'OVERALL': 1,
             'OWN': 1,
             'PARTNER': 2,
             'PARTNERS': 2,
             'PASSION': 1,
             'PATTERNS': 1,
             'PERFORMANCE': 5,
             'PERIODIC': 1,
             'PIVOT': 1,
             'POWER': 3,
             'POWERPOINT': 2,
             'PRACTICES': 2,
             'PREFERRED': 3,
             'PRESENTATION': 3,
             'PRIMARY': 1,
             'PRINTOUTS': 1,
             'PRIORITIZE': 1,
             'PROBLEM': 2,
             'PROBLEMS': 2,
             'PROCESS': 3,
             'PRODUCT': 2,
             'PRODUCTS': 1,
             'PROFESSIONAL': 2,
             'PROFICIENCY': 1,
             'PROFICIENT': 1,
             'PROGRAMMING': 1,
             'PROJECTS': 3,
             'PROMOTES': 2,
             'PROVEN': 1,
             'PROVIDE': 3,
             'PROVIDING': 3,
             'QUALIFICATION': 1,
             'QUALIFICATIONS': 4,
             'QUALITY': 1,
             'QUANTITATIVE': 1,
             'RECALL': 1,
             'RECONCILIATION': 1,
             'RECORDS': 1,
             'REGULATIONS': 1,
             'REGULATORY': 1,
             'RELATE': 1,
             'RELATED': 1,
             'REMEDIATION': 1,
             'REPORT': 1,
             'REPORTING': 9,
             'REPORTS': 2,
             'REQUIRED': 1,
             'REQUIREMENTS': 1,
             'RESOURCES': 1,
             'RESPONSIBILITIES': 1,
             'RESULTS': 1,
             'RETAIN': 1,
             'REVIEWING': 1,
             'REVIEWS': 1,
             'RHYTHM': 1,
             'RISK': 1,
             'ROADMAP': 1,
             'ROB': 1,
             'ROLE': 1,
             'SALES': 2,
             'SCENARIO': 1,
             'SCENARIOS': 1,
             'SCIENCE': 3,
             'SCORECARD': 2,
             'SCRIPTING': 1,
             'SCRIPTS': 1,
             'SECONDARY': 1,
             'SECTOR': 1,
             'SELF': 2,
             'SENIOR': 1,
             'SERVICES': 1,
             'SETS': 1,
             'SHOW': 2,
             'SIGNALS': 1,
             'SIMPLE': 1,
             'SKILLS': 13,
             'SOFTWARE': 1,
             'SOLVING': 2,
             'SOURCES': 3,
             'SPACE': 1,
             'SPARK': 1,
             'SPECIAL': 1,
             'SPECIFIC': 1,
             'SQL': 3,
             'STAKEHOLDERS': 3,
             'STANDARD': 1,
             'STANDARDS': 2,
             'STATE': 1,
             'STATISTICAL': 2,
             'STATISTICS': 1,
             'STRATEGICALLY': 1,
             'STRATEGIES': 1,
             'STRONG': 8,
             'SUCCESS': 2,
             'SUCCESSFUL': 1,
             'SUCH': 3,
             'SUPPLEMENTED': 1,
             'SYSTEMS': 2,
             'TABLES': 1,
             'TACKLE': 1,
             'TAKEN': 1,
             'TARGETS': 1,
             'TEAM': 5,
             'TECHNICAL': 2,
             'TECHNIQUES': 2,
             'TECHNOLOGIES': 1,
             'THAT': 6,
             'THE': 8,
             'THESE': 2,
             'THINK': 1,
             'THIS': 3,
             'THRIVE': 1,
             'TO': 34,
             'TOOLS': 6,
             'TRACK': 1,
             'TRANSLATE': 1,
             'TREND': 1,
             'TRENDS': 2,
             'UNDERSTOOD': 1,
             'UNIVERSITY': 1,
             'UPON': 1,
             'USERS': 1,
             'USING': 4,
             'UTILIZE': 1,
             'VALUES': 1,
             'VARIANCE': 1,
             'VARIETY': 1,
             'VARYING': 1,
             'VERBAL': 2,
             'VIEWS': 1,
             'VISION': 1,
             'VISUALIZATION': 1,
             'VOLUME': 1,
             'WAYS': 1,
             'WELL': 2,
             'WHAT': 1,
             'WHICH': 1,
             'WIDE': 2,
             'WILL': 1,
             'WILLINGNESS': 1,
             'WITH': 23,
             'WITHIN': 1,
             'WORK': 9,
             'WORKFLOWS': 1,
             'WORKING': 5,
             'WORKSTREAM': 1,
             'WRITTEN': 4,
             'YEARS': 3,
             'YOU': 2})




```python
import operator
max(word_counts.items(), key=operator.itemgetter(1))[0]
```




    'AND'




```python
x = Counter(word_counts)
y =OrderedDict(x.most_common())
```


```python
y
```




    OrderedDict([('AND', 58),
                 ('TO', 34),
                 ('DATA', 33),
                 ('WITH', 23),
                 ('OF', 18),
                 ('IN', 17),
                 ('BUSINESS', 13),
                 ('SKILLS', 13),
                 ('OR', 11),
                 ('ON', 11),
                 ('A', 10),
                 ('WORK', 9),
                 ('REPORTING', 9),
                 ('ABILITY', 9),
                 ('THE', 8),
                 ('STRONG', 8),
                 ('ANALYSIS', 7),
                 ('THAT', 6),
                 ('TOOLS', 6),
                 ('AS', 6),
                 ('OUR', 6),
                 ('DEVELOP', 5),
                 ('ANALYTICS', 5),
                 ('PERFORMANCE', 5),
                 ('FOR', 5),
                 ('EXPERIENCE', 5),
                 ('ENGINEERING', 5),
                 ('ADVANCED', 5),
                 ('WORKING', 5),
                 ('TEAM', 5),
                 ('USING', 4),
                 ('FROM', 4),
                 ('MANAGEMENT', 4),
                 ('DEFINE', 4),
                 ('WRITTEN', 4),
                 ('EXCEL', 4),
                 ('INSIGHTS', 4),
                 ('QUALIFICATIONS', 4),
                 ('BE', 4),
                 ('ANALYZE', 3),
                 ('PROVIDE', 3),
                 ('SOURCES', 3),
                 ('INFORMATION', 3),
                 ('NEEDS', 3),
                 ('PROCESS', 3),
                 ('STAKEHOLDERS', 3),
                 ('ACROSS', 3),
                 ('ORGANIZATION', 3),
                 ('LEAD', 3),
                 ('COMPLIANCE', 3),
                 ('ENSURE', 3),
                 ('IS', 3),
                 ('YEARS', 3),
                 ('PRESENTATION', 3),
                 ('MINING', 3),
                 ('LARGE', 3),
                 ('SQL', 3),
                 ('SUCH', 3),
                 ('METRICS', 3),
                 ('DELIVER', 3),
                 ('SCIENCE', 3),
                 ('PREFERRED', 3),
                 ('EXCEPTIONAL', 3),
                 ('KNOWLEDGE', 3),
                 ('POWER', 3),
                 ('BI', 3),
                 ('DRIVEN', 3),
                 ('PROJECTS', 3),
                 ('ENVIRONMENT', 3),
                 ('COLLABORATION', 3),
                 ('FINANCE', 3),
                 ('THIS', 3),
                 ('PROVIDING', 3),
                 ('CLEAR', 3),
                 ('INTERPRET', 2),
                 ('STATISTICAL', 2),
                 ('TECHNIQUES', 2),
                 ('ONGOING', 2),
                 ('REPORTS', 2),
                 ('DATABASES', 2),
                 ('SYSTEMS', 2),
                 ('OTHER', 2),
                 ('TRENDS', 2),
                 ('COMPLEX', 2),
                 ('COMPUTER', 2),
                 ('LOCATE', 2),
                 ('PROBLEMS', 2),
                 ('NEW', 2),
                 ('CREATE', 2),
                 ('BASED', 2),
                 ('ACTIVITIES', 2),
                 ('STANDARDS', 2),
                 ('PARTNERS', 2),
                 ('BEING', 2),
                 ('MINIMUM', 2),
                 ('THESE', 2),
                 ('COMMUNICATIONS', 2),
                 ('ORDER', 2),
                 ('BEST', 2),
                 ('PRACTICES', 2),
                 ('MUST', 2),
                 ('OFFICE', 2),
                 ('EXPERTISE', 2),
                 ('ARE', 2),
                 ('KEY', 2),
                 ('DEGREE', 2),
                 ('PROFESSIONAL', 2),
                 ('CERTIFIED', 2),
                 ('HIGH', 2),
                 ('HEALTH', 2),
                 ('WELL', 2),
                 ('FORECASTS', 2),
                 ('DESIGN', 2),
                 ('DASHBOARDS', 2),
                 ('ACTIONABLE', 2),
                 ('IMPROVE', 2),
                 ('PRODUCT', 2),
                 ('3', 2),
                 ('HANDS', 2),
                 ('PROBLEM', 2),
                 ('SOLVING', 2),
                 ('TECHNICAL', 2),
                 ('MANIPULATION', 2),
                 ('POWERPOINT', 2),
                 ('FAMILIARITY', 2),
                 ('VERBAL', 2),
                 ('COMMUNICATION', 2),
                 ('SELF', 2),
                 ('SHOW', 2),
                 ('AMBIGUOUS', 2),
                 ('INCOMPLETE', 2),
                 ('DIRTY', 2),
                 ('PROMOTES', 2),
                 ('DEVELOPMENT', 2),
                 ('FUNCTIONS', 2),
                 ('ETC', 2),
                 ('SALES', 2),
                 ('MARKETING', 2),
                 ('INCLUDES', 2),
                 ('WIDE', 2),
                 ('ACTIONS', 2),
                 ('SCORECARD', 2),
                 ('YOU', 2),
                 ('CAN', 2),
                 ('SUCCESS', 2),
                 ('PARTNER', 2),
                 ('INTO', 2),
                 ('RESULTS', 1),
                 ('IMPLEMENT', 1),
                 ('COLLECTION', 1),
                 ('STRATEGIES', 1),
                 ('OPTIMIZE', 1),
                 ('EFFICIENCY', 1),
                 ('QUALITY', 1),
                 ('ACQUIRE', 1),
                 ('PRIMARY', 1),
                 ('SECONDARY', 1),
                 ('MAINTAIN', 1),
                 ('IDENTIFY', 1),
                 ('PATTERNS', 1),
                 ('SETS', 1),
                 ('FILTER', 1),
                 ('CLEAN', 1),
                 ('BY', 1),
                 ('REVIEWING', 1),
                 ('PRINTOUTS', 1),
                 ('INDICATORS', 1),
                 ('CORRECT', 1),
                 ('CODE', 1),
                 ('PRIORITIZE', 1),
                 ('IMPROVEMENT', 1),
                 ('OPPORTUNITIES', 1),
                 ('MULTIPLE', 1),
                 ('WORKFLOWS', 1),
                 ('CHANGING', 1),
                 ('REMEDIATION', 1),
                 ('WORKSTREAM', 1),
                 ('INCLUDING', 1),
                 ('IMPACTS', 1),
                 ('EVALUATION', 1),
                 ('ESTABLISH', 1),
                 ('CONNECTIVITY', 1),
                 ('CONTACTS', 1),
                 ('AWARENESS', 1),
                 ('PERIODIC', 1),
                 ('RECONCILIATION', 1),
                 ('GATHERED', 1),
                 ('STANDARD', 1),
                 ('5', 1),
                 ('7', 1),
                 ('WITHIN', 1),
                 ('BANKING', 1),
                 ('FINANCIAL', 1),
                 ('SECTOR', 1),
                 ('FOCUS', 1),
                 ('FEDERAL', 1),
                 ('STATE', 1),
                 ('LAWS', 1),
                 ('REGULATIONS', 1),
                 ('RISK', 1),
                 ('COMBINATION', 1),
                 ('DISCIPLINES', 1),
                 ('METICULOUS', 1),
                 ('ATTENTION', 1),
                 ('DETAIL', 1),
                 ('INTERACT', 1),
                 ('INFLUENCE', 1),
                 ('SENIOR', 1),
                 ('EXECUTIVE', 1),
                 ('ACHIEVE', 1),
                 ('OPTIMAL', 1),
                 ('OUTCOMES', 1),
                 ('MS', 1),
                 ('UTILIZE', 1),
                 ('KEEP', 1),
                 ('ORGANIZED', 1),
                 ('RECORDS', 1),
                 ('INVENTORIES', 1),
                 ('ADVANCE', 1),
                 ('AD', 1),
                 ('RETAIN', 1),
                 ('RECALL', 1),
                 ('UPON', 1),
                 ('MET', 1),
                 ('CONTINUALLY', 1),
                 ('IMPROVED', 1),
                 ('DEMONSTRATED', 1),
                 ('INITIATIVES', 1),
                 ('THINK', 1),
                 ('STRATEGICALLY', 1),
                 ('COLLABORATE', 1),
                 ('DEVELOPING', 1),
                 ('ACADEMIC', 1),
                 ('CREDENTIALS', 1),
                 ('BA', 1),
                 ('BS', 1),
                 ('AN', 1),
                 ('ACCREDITED', 1),
                 ('UNIVERSITY', 1),
                 ('QUALIFICATION', 1),
                 ('REGULATORY', 1),
                 ('MANGER', 1),
                 ('INTERNAL', 1),
                 ('AUDITOR', 1),
                 ('RELATED', 1),
                 ('CERTIFICATION', 1),
                 ('MANIPULATE', 1),
                 ('VOLUME', 1),
                 ('DIMENSIONALITY', 1),
                 ('VARYING', 1),
                 ('VARIETY', 1),
                 ('TRACK', 1),
                 ('REPORT', 1),
                 ('ASSESS', 1),
                 ('OVERALL', 1),
                 ('DEPTH', 1),
                 ('FUNNELS', 1),
                 ('DRIVERS', 1),
                 ('TARGETS', 1),
                 ('BUILD', 1),
                 ('ENABLE', 1),
                 ('MONITORING', 1),
                 ('ENGINEERS', 1),
                 ('OPERATIONALIZE', 1),
                 ('INGEST', 1),
                 ('HOW', 1),
                 ('BACHELOR', 1),
                 ('MASTERS', 1),
                 ('STATISTICS', 1),
                 ('MATH', 1),
                 ('ECONOMICS', 1),
                 ('IDEAL', 1),
                 ('MBA', 1),
                 ('PROGRAMMING', 1),
                 ('SCRIPTING', 1),
                 ('PROFICIENT', 1),
                 ('C', 1),
                 ('SCRIPTS', 1),
                 ('NOT', 1),
                 ('REQUIRED', 1),
                 ('DASHBOARD', 1),
                 ('PROFICIENCY', 1),
                 ('E', 1),
                 ('G', 1),
                 ('CREATING', 1),
                 ('FORMULAS', 1),
                 ('PIVOT', 1),
                 ('TABLES', 1),
                 ('BIG', 1),
                 ('TECHNOLOGIES', 1),
                 ('AZURE', 1),
                 ('LAKE', 1),
                 ('HADOOP', 1),
                 ('SPARK', 1),
                 ('HIVE', 1),
                 ('DOMAIN', 1),
                 ('AREAS', 1),
                 ('SOFTWARE', 1),
                 ('PASSION', 1),
                 ('GAMING', 1),
                 ('SPACE', 1),
                 ('RELATE', 1),
                 ('USERS', 1),
                 ('HOLISTIC', 1),
                 ('M365', 1),
                 ('CUSTOMER', 1),
                 ('ARRAY', 1),
                 ('AUDIENCES', 1),
                 ('CONSULTATIVE', 1),
                 ('MANNER', 1),
                 ('DETERMINE', 1),
                 ('CURRENT', 1),
                 ('FUTURE', 1),
                 ('OUTLOOKS', 1),
                 ('INTELLIGENCE', 1),
                 ('WILL', 1),
                 ('MAKE', 1),
                 ('PRODUCTS', 1),
                 ('SERVICES', 1),
                 ('BETTER', 1),
                 ('GROW', 1),
                 ('SUCCESSFUL', 1),
                 ('ROLE', 1),
                 ('HAVE', 1),
                 ('ORAL', 1),
                 ('DO', 1),
                 ('ATTITUDE', 1),
                 ('WILLINGNESS', 1),
                 ('TACKLE', 1),
                 ('HARD', 1),
                 ('INNOVATIVE', 1),
                 ('WAYS', 1),
                 ('ALSO', 1),
                 ('THRIVE', 1),
                 ('VALUES', 1),
                 ('CROSS', 1),
                 ('BUILDING', 1),
                 ('OTHERS', 1),
                 ('RESPONSIBILITIES', 1),
                 ('SPECIFIC', 1),
                 ('DOMAINS', 1),
                 ('SIGNALS', 1),
                 ('WHICH', 1),
                 ('TRANSLATE', 1),
                 ('SIMPLE', 1),
                 ('EASILY', 1),
                 ('UNDERSTOOD', 1),
                 ('RESOURCES', 1),
                 ('REQUIREMENTS', 1),
                 ('VIEWS', 1),
                 ('ROADMAP', 1),
                 ('CAPABILITIES', 1),
                 ('MEET', 1),
                 ('VISION', 1),
                 ('SCENARIOS', 1),
                 ('DEEP', 1),
                 ('DIVES', 1),
                 ('ANALYSES', 1),
                 ('METRIC', 1),
                 ('SPECIAL', 1),
                 ('OWN', 1),
                 ('RHYTHM', 1),
                 ('ROB', 1),
                 ('MEETINGS', 1),
                 ('MBRS', 1),
                 ('REVIEWS', 1),
                 ('TREND', 1),
                 ('VARIANCE', 1),
                 ('COMMENTARY', 1),
                 ('FINDINGS', 1),
                 ('ARTICULATE', 1),
                 ('WHAT', 1),
                 ('TAKEN', 1),
                 ('EACH', 1),
                 ('SCENARIO', 1),
                 ('DRIVE', 1),
                 ('BASIC', 1),
                 ('SUPPLEMENTED', 1),
                 ('LEARNINGS', 1),
                 ('PROVEN', 1),
                 ('QUANTITATIVE', 1),
                 ('ORIENTATION', 1),
                 ('VISUALIZATION', 1),
                 ('GREAT', 1)])




```python
import csv
with open("my_output_file.txt", "w") as f:
   writer = csv.writer(f, delimiter=":")
   writer.writerows(y)
```
