def chunkIt(seq, num):
    avg = len(seq) / float(num)
    out = []
    last = 0.0

    while last < len(seq):
        out.append(seq[int(last):int(last + avg)])
        last += avg

    return out

class Token:
	
	def __init__(self, separator='\t', sent_id="NONE", text="NONE"):
		self.id = "0"
		self.word = "_"
		self.lemma = "_"
		self.upos = "_"
		self.xpos = "_"
		self.feats = "_"
		self.dephead = "0"
		self.deprel = "_"
		self.deps = "_"
		self.misc = "_"
		self.children = []
		self.separator = separator
		self.col = dict()
		self.sent_id = sent_id
		self.text = text
		self.color = ""

	def build(self, txt):
		coluna = txt.split(self.separator)

		self.id = coluna[0]
		self.word = coluna[1]
		self.lemma = coluna[2]
		self.upos = coluna[3]
		self.xpos = coluna[4]
		self.feats = coluna[5]
		self.dephead = coluna[6]
		self.deprel = coluna[7]
		self.deps = coluna[8]
		self.misc = coluna[9]
		self.col["id"] = self.id
		self.col["word"] = self.word
		self.col["lemma"] = self.lemma
		self.col["upos"] = self.upos
		self.col["xpos"] = self.xpos
		self.col["feats"] = self.feats
		self.col["dephead"] = self.dephead
		self.col["deprel"] = self.deprel
		self.col["deps"] = self.deps
		self.col["misc"] = self.misc
		self.col["text"] = self.text

	def to_str(self):
		return self.separator.join([self.id, self.word, self.lemma, self.upos, self.xpos, self.feats, self.dephead, self.deprel, self.deps, self.misc])


class Sentence:

	def __init__(self, separator="\n", recursivo=True):
		self.text = ""
		self.sent_id = ""
		self.source = ""
		self.id = ""
		self.metadados = {}
		self.recursivo = recursivo

		f = "_\t" * 10
		self.default_token = Token()
		self.default_token.build(f.rsplit('\t', 1)[0])

		self.tokens = list()
		self.tokens_incompletos = list()
		self.separator = separator


	def get_head(self, token):
		next_t = False
		previous_t = False
		for tok in self.tokens:
			if tok.id == token.dephead:
				token.head_token = tok
				break

		for tok in self.tokens:
			if not "-" in token.id and not "-" in tok.id and not "/" in token.id and not "/" in tok.id and not ">" in tok.id and not ">" in token.id:
				if int(token.id) == int(tok.id) - 1:
					token.next_token = tok
					next_t = True
				if int(token.id) == int(tok.id) + 1:
					token.previous_token = tok
					previous_t = True
			if next_t and previous_t:
				break

		return token

	def build(self, txt):
		if '# text =' in txt:
			self.text = txt.split('# text =')[1].split('\n')[0].strip()
			self.metadados['text'] = self.text
		if '# sent_id =' in txt:
			self.sent_id = txt.split('# sent_id =')[1].split('\n')[0].strip()
			self.metadados['sent_id'] = self.sent_id
		if '# source =' in txt:
			self.source = txt.split('# source =')[1].split('\n')[0].strip()
			self.metadados['source'] = self.source
		if '# id =' in txt:
			self.id = txt.split('# id =')[1].split('\n')[0].strip()
			self.metadados["id"] = self.id
		
		for linha in txt.split(self.separator):
			if linha and "#" == linha[0] and "=" in linha:
				identificador = linha.split("#", 1)[1].split('=', 1)[0].strip()
				if identificador not in ["text", "sent_id", "source", "id"]:
					valor = linha.split('=', 1)[1].strip()
					self.metadados[identificador] = valor
			if "\t" in linha:
				tok = Token(sent_id = self.sent_id, text = self.text)
				tok.build(linha)
				tok.head_token = self.default_token
				tok.next_token = self.default_token
				tok.previous_token = self.default_token
				self.tokens.append(tok)


		if self.recursivo != False:
			for token in self.tokens:
				token = self.get_head(token)

	def tokens_to_str(self):
		return "\n".join([tok.to_str() for tok in self.tokens])

	def metadados_to_str(self):
		return "\n".join(["# " + x + " = " + self.metadados[x] for x in self.metadados])

	def to_str(self):
		return self.metadados_to_str() + "\n" + self.tokens_to_str()


class Corpus:

	def __init__(self, separator="\n\n", recursivo=True, sent_id=None, thread=False):
		self.len = 0
		self.sentences = {}
		self.separator = separator
		self.sent_list = []
		self.recursivo = recursivo
		self.sent_id = sent_id
		self.pre = ""
		self.pos = ""
		self.thread = thread

	def build(self, txt):
		if self.sent_id:
			import re
			old_txt = txt
			txt = re.search(r"(\n\n|^).*?# sent_id = " + self.sent_id + r"\n.*?(\n\n|$)", txt, flags=re.DOTALL)[0].strip()
			if '\n\n' in txt: txt = txt.rsplit("\n\n", 1)[1]
			self.pre = old_txt.split(txt)[0].strip()
			self.pos = old_txt.split(txt)[1].strip()

		sents = txt.split(self.separator)

		if self.thread:
			import threading
			threads = {}
			chunks = chunkIt(sents, self.thread)
			for i in range(self.thread):
				threads[i] = threading.Thread(target=self.sent_build, args=(chunks[i],))
			for i in range(self.thread):
				threads[i].start()
			for i in range(self.thread):
				threads[i].join()

		else:
			self.sent_build(sents)

		self.len = len(self.sentences)

	def sent_build(self, sents):
		for sentence in sents:
			sent = Sentence(recursivo=self.recursivo)
			sent.build(sentence)
			self.sent_list.append(sent)
			if sent.sent_id:
				self.sentences[sent.sent_id] = sent
			elif sent.id:
				self.sentences[sent.id] = sent
			elif sent.text:
				self.sentences[sent.text] = sent


	def to_str(self):
		retorno = list()

		for sentence in self.sentences.values():
			retorno.append(sentence.to_str())
		
		return "\n\n".join(retorno) + '\n\n'

	def load(self, path):
		with open(path, "r") as f:
			self.build(f.read())

	def save(self, path):
		final = self.to_str() if not self.sent_id else (self.pre + "\n\n" + self.to_str() + self.pos).strip() + "\n\n"
		with open(path, "w") as f:
			f.write(final)
