
describe('Grid', () => {
  beforeEach(() => {
    cy.visit('')
  })

  it('has square tiles of uniform size', () => {
    cy.get('.replay-dropdown')
      .select('1')

    cy.get('.grid')
      .trigger('wheel', 'center', {
        force: true,
        deltaY: -500,
      })
      .find('td')
      .each($cell => {
        cy.wrap($cell).css()
        expect(style.width).to.equal(style.height)

          // cy.wrap($cell)
          //   .should('have.css', 'width', '29.6px')
          //   .should('have.css', 'height', '29.6px')
      })
  })
})
