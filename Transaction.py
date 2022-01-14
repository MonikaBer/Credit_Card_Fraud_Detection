class Transaction:
    def __init__(self, attributes, status):
        self.attributes = attributes
        self.status = status  # class (0/1)

    def show(self):
        s = ''
        for attr in self.attributes:
            s += f'{attr},'
        print(s)
