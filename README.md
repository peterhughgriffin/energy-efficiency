# EPC Plotter

This project uses R to explore data relating to UK housing stock, energy eficiency and fuel poverty.

The primary aim is to examine EPC certificates from a given locale. GetEPC.R accesses certificates from the MHCLG [Open Data Communities website](https://epc.opendatacommunities.org/docs/api/domestic) using the API. I'm working on tools that can then plot various aspects of these data to identify what sort of energy efficiency measures could be best used in an area.

To access their data you need to create a free account and then make a file called 'Credentials.csv' to store your login and API key in. The first line of this should be: "api_key,user,pass" and the second line should be each of those credentials separated by commas.

ProcessLSOA.R is a tool to examine some data that I have been exploring using QGIS. This probably doesn't make much sense without the QGIS project, but is included for my personal use.

## Author

**Peter Griffin**

## License
This software is released under an MIT License

Copyright (c) 2019 Peter Griffin

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
