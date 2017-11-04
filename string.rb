class String
  def numeric?
    Integer(self) != nil rescue false
  end
  def alnum?
    !!match(/^[[:alnum:]]+$/)
  end
  def alpha?
    !!match(/^[[:alpha:]]+$/)
  end
end
