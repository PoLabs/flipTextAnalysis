form.checkBox({name: "formdospell", label: "Correct spelling"});
form.checkBox({name: "formdostem", label: "Perform stemming"});
form.textBox({name: "formreplacewords", label: "Replace these words (e.g. great:good, terrible:bad)", required: false});
form.textBox({name: "formremovewords", label: "Remove these words (in addition to stopwords)", required: false});
form.numericUpDown({name: "formminfreq", label: "Minimum word count:", increment: 1, minimum: 1});