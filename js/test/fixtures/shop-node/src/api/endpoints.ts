export const SHOP_ENDPOINTS = {
  /** @atlas.source/id :endpoint-call/order */
  order: (userId: string, orderId: string) => `/resource/shop.users.${userId}.orders/${orderId}`,
  /** @atlas.source/id :endpoint-call/place-order */
  placeOrder: (userId: string) => `/resource/shop.users.${userId}/orders`,
  /** @atlas.source/id :endpoint-call/refund */
  refund: (orderId: string) => `/resource/shop.orders.${orderId}/refunds`,
  /** @atlas.source/id :endpoint-call/untracked */
  untracked: () => '/shop/untracked',
  cart: (userId: string) => `/resource/shop.users.${userId}/cart`,
  orders: (userId: string) => `/resource/shop.users.${userId}/orders`,
  legacy: '/v1/legacy',
};
