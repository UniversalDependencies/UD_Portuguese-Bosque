class Token:
	
	def __init__(self, separator='\t'):
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
		#self.head_token = ""

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

	def to_str(self):
		return self.separator.join([self.id, self.word, self.lemma, self.upos, self.xpos, self.feats, self.dephead, self.deprel, self.deps, self.misc])


class Sentence:

	def __init__(self, separator="\n", recursivo=False):
		self.text = ""
		self.sent_id = ""
		self.source = ""
		self.id = ""
		self.metadados = []
		self.recursivo = recursivo

		f = "_\t" * 10
		self.default_token = Token()
		self.default_token.build(f.rsplit('\t', 1)[0])

		self.tokens = list()
		self.tokens_incompletos = list()
		self.separator = separator

	def get_head(self, token):
		for tok in self.tokens_incompletos:
			if tok.id == token.dephead:
				token.head_token = tok
				break
			token.head_token = self.default_token

		return token

	def build(self, txt):
		if '# text = ' in txt:
			self.text = txt.split('# text = ')[1].split('\n')[0]
		if '# sent_id = ' in txt:
			self.sent_id = txt.split('# sent_id = ')[1].split('\n')[0]
		if '# source = ' in txt:
			self.source = txt.split('# source = ')[1].split('\n')[0]
		if '# id = ' in txt:
			self.id = txt.split('# id = ')[1].split('\n')[0]
		
		tokens_incompletos = list()
		for linha in txt.split(self.separator):
			if linha and "#" == linha[0]:
				self.metadados.append(linha)
			if "\t" in linha:
				tok = Token()
				tok.build(linha)
				if not self.recursivo: self.tokens.append(tok)
				if self.recursivo: self.tokens_incompletos.append(tok)

		if not self.recursivo:
			for token in self.tokens:
				for _token in self.tokens:
					if token.dephead == _token.id:
						token.head_token = _token
						break
					token.head_token = self.default_token
		
		if self.recursivo:
			for token in self.tokens_incompletos:
				self.tokens.append(self.get_head(token))

			for i in range(4):
				for token in self.tokens:
					token = self.get_head(token)

	def tokens_to_str(self):
		lista = list()

		for tok in self.tokens:
			lista.append(tok.to_str())

		return lista

	def to_str(self):
		return "\n".join(self.metadados) + "\n" + "\n".join(self.tokens_to_str())


class Corpus:

	def __init__(self, separator="\n\n", recursivo=False):
		self.len = 0
		self.sentences = {}
		self.separator = separator
		self.sent_list = []
		self.recursivo = recursivo

	def build(self, txt):
		sents = txt.split(self.separator)
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

		self.len = len(self.sentences)

	def to_str(self):
		retorno = list()

		for sentence in self.sentences.values():
			retorno.append(sentence.to_str())
		
		return "\n\n".join(retorno) + '\n'